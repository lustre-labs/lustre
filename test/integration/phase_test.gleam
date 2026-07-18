// IMPORTS ---------------------------------------------------------------------

@target(javascript)
import agnostic
@target(javascript)
import agnostic/effect.{type Effect}
@target(javascript)
import agnostic/element.{type Element}
@target(javascript)
import agnostic/element/html
@target(javascript)
import agnostic/platform
@target(javascript)
import agnostic/platform/dom
@target(javascript)
import gleam/dynamic.{type Dynamic}
@target(javascript)
import gleam/int
@target(javascript)
import lustre_test

// TYPES -----------------------------------------------------------------------

@target(javascript)
pub type Runtime(message, model)

@target(javascript)
type Message {
  Incr
  SetTo(Int)
}

// HELPERS ---------------------------------------------------------------------

@target(javascript)
fn get_platform() -> platform.Platform(
  dom.DomNode,
  dom.DomNode,
  dom.DomNode,
  dom.DomEvent,
  message,
  dom.DomNode,
) {
  let assert Ok(p) = dom.platform("body")
  p
}

@target(javascript)
fn view(model: Int) -> Element(Message) {
  html.p([], [html.text("count:" <> int.to_string(model))])
}

@target(javascript)
fn update(model: Int, message: Message) -> #(Int, Effect(Message)) {
  case message {
    Incr -> #(model + 1, effect.none())
    SetTo(n) -> #(n, effect.none())
  }
}

// CUSTOM PLATFORM PHASE TESTS -------------------------------------------------

@target(javascript)
pub fn custom_platform_phase_order_test() {
  use <- lustre_test.test_filter("custom_platform_phase_order_test")

  let init = fn(_) {
    #(
      0,
      effect.batch([
        effect.deferred("two", fn(_, _) { log_push("task:two") }),
        effect.deferred("one", fn(_, _) { log_push("task:one:a") }),
        effect.deferred("one", fn(_, _) { log_push("task:one:b") }),
        effect.deferred("undeclared", fn(_, _) { log_push("task:undeclared") }),
        effect.from(fn(_) { log_push("sync") }),
      ]),
    )
  }

  use _runtime <- with_custom_phase_runtime(
    dom.to_string(view(0)),
    fn() { agnostic.application(init, update, view) },
    ["one", "two"],
  )

  // - the synchronous task runs during effect processing, before the render;
  // - schedulers are invoked in the platform's declaration order (one, two),
  //   once each, regardless of the order the effects were batched in;
  // - each phase's tasks run in scheduling order (`effect.batch` accumulates
  //   by prepending, so tasks within a phase are scheduled in reverse batch
  //   order — the runtime documents no ordering guarantees between batched
  //   effects);
  // - the task tagged with an undeclared phase is silently dropped.
  assert take_log()
    == [
      "sync", "schedule:one", "schedule:two", "task:one:b", "task:one:a",
      "task:two",
    ]
}

@target(javascript)
pub fn custom_platform_phase_dispatch_and_root_test() {
  use <- lustre_test.test_filter("custom_platform_phase_dispatch_and_root_test")

  let init = fn(_) {
    #(
      0,
      effect.deferred("one", fn(dispatch, root) {
        case is_body(root) {
          True -> log_push("root:body")
          False -> log_push("root:other")
        }
        dispatch(SetTo(7))
      }),
    )
  }

  use runtime <- with_custom_phase_runtime(
    dom.to_string(view(0)),
    fn() { agnostic.application(init, update, view) },
    ["one", "two"],
  )

  // The task received dispatch and the platform root, and its synchronous
  // dispatch triggered exactly one further update + render. The second render
  // had no pending tasks, so the scheduler was only invoked once.
  assert get_model(runtime) == 7
  assert render_count(runtime) == 2
  assert take_log() == ["schedule:one", "root:body"]
}

@target(javascript)
pub fn custom_platform_phase_map_test() {
  use <- lustre_test.test_filter("custom_platform_phase_map_test")

  let init = fn(_) {
    #(
      0,
      effect.map(effect.deferred("one", fn(dispatch, _) { dispatch(3) }), fn(n) {
        SetTo(n + 1)
      }),
    )
  }

  use runtime <- with_custom_phase_runtime(
    dom.to_string(view(0)),
    fn() { agnostic.application(init, update, view) },
    ["one", "two"],
  )

  // The deferred task survived `effect.map` with its phase tag intact and
  // delivered the mapped message.
  assert get_model(runtime) == 4
}

// DOM PLATFORM PHASE TESTS ----------------------------------------------------

@target(javascript)
pub fn dom_before_paint_observes_updated_dom_test() {
  use <- lustre_test.test_filter("dom_before_paint_observes_updated_dom_test")

  let _ = take_log()

  let init = fn(_) {
    #(
      0,
      effect.batch([
        dom.before_paint(fn(_, _) { log_push("before_paint:" <> body_text()) }),
        dom.after_paint(fn(_, _) { log_push("after_paint") }),
      ]),
    )
  }

  // The initial HTML renders count:-1; the runtime's first render updates the
  // DOM to count:0 before any phase effect runs.
  use _runtime <- with_client_runtime(
    dom.to_string(view(-1)),
    fn() { agnostic.application(init, update, view) },
    get_platform,
  )

  // `before_paint` observed the updated DOM, and ran before the `after_paint`
  // task from the same render.
  assert take_log() == ["before_paint:count:0", "after_paint"]
}

@target(javascript)
pub fn dom_second_render_before_paint_test() {
  use <- lustre_test.test_filter("dom_second_render_before_paint_test")

  let _ = take_log()

  let init = fn(_) {
    #(
      0,
      effect.batch([
        dom.before_paint(fn(dispatch, _) {
          log_push("before_paint:init")
          dispatch(Incr)
        }),
        dom.after_paint(fn(_, _) { log_push("after_paint:init") }),
      ]),
    )
  }

  let update = fn(model, message) {
    case message {
      Incr -> #(
        model + 1,
        dom.before_paint(fn(_, _) { log_push("before_paint:second") }),
      )
      SetTo(n) -> #(n, effect.none())
    }
  }

  use runtime <- with_client_runtime(
    dom.to_string(view(0)),
    fn() { agnostic.application(init, update, view) },
    get_platform,
  )

  // A dispatch inside `before_paint` produces a second render whose own
  // `before_paint` batch runs before the original `after_paint` rAF callback —
  // upstream's second-render-before-paint ordering.
  assert get_model(runtime) == 1
  assert take_log()
    == ["before_paint:init", "before_paint:second", "after_paint:init"]
}

// HEADLESS DROP TESTS ---------------------------------------------------------

@target(javascript)
pub fn headless_drops_deferred_effects_test() {
  use <- lustre_test.test_filter("headless_drops_deferred_effects_test")

  let _ = take_log()

  let init = fn(_) {
    #(
      0,
      effect.batch([
        effect.from(fn(_) { log_push("sync") }),
        dom.before_paint(fn(_, _) { log_push("deferred") }),
      ]),
    )
  }

  let app = agnostic.application(init, update, view)
  let assert Ok(_runtime) =
    agnostic.start(app, on: platform.headless(), with: 0)

  use <- flush_microtasks

  // The synchronous effect ran; the deferred effect was dropped — headless
  // platforms declare no phases.
  assert take_log() == ["sync"]
}

// FFI ------------------------------------------------------------------------

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "with_custom_phase_runtime")
fn with_custom_phase_runtime(
  initial_html: String,
  make_app: fn() -> agnostic.App(Nil, model, message),
  phase_names: List(String),
  test_callback: fn(Runtime(message, model)) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "with_client_runtime")
fn with_client_runtime(
  initial_html: String,
  make_app: fn() -> agnostic.App(Nil, model, message),
  get_platform: fn() ->
    platform.Platform(
      dom.DomNode,
      dom.DomNode,
      dom.DomNode,
      dom.DomEvent,
      message,
      dom.DomNode,
    ),
  test_callback: fn(Runtime(message, model)) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "model")
fn get_model(runtime: Runtime(message, model)) -> model

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "render_count")
fn render_count(runtime: Runtime(message, model)) -> Int

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "log_push")
fn log_push(entry: String) -> Nil

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "take_log")
fn take_log() -> List(String)

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "body_text")
fn body_text() -> String

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "is_body")
fn is_body(node: Dynamic) -> Bool

@target(javascript)
@external(javascript, "./phase_test.ffi.mjs", "flush_microtasks")
fn flush_microtasks(callback: fn() -> Nil) -> Nil
