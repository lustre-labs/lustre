// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Selector}
import gleam/json.{type Json}
import gleam/list

// TYPES -----------------------------------------------------------------------

///
///
pub type Effect(message) {
  /// We have a `None` variant even though it's basically the same as batching an
  /// empty list because it's  more convenient for the js runtime to check if an
  /// effect is none.
  None
  Batch(all: List(Effect(message)))
  Effect(run: fn(Actions(message)) -> Nil, kind: Kind)
  /// Effects aren't monadic for consumers, but we need a `then`-like way of accessing
  /// actions specifically so we can generate cleanup keys for disposable effects.
  /// When performing effects this function is immediately called with other sync
  /// effects, but the returned effect is only run if itself is synchronous, otherwise
  /// it's batched with the other timing-based effects.
  Deferred(run: fn(Actions(message)) -> Effect(message))
}

///
///
pub type Kind {
  Synchronous
  BeforePaint
  AfterPaint
}

///
///
pub type Key

/// A record of effectful functions that the runtime provides to effects when
/// they are run. These are often various ways to interact with the runtime.
///
pub type Actions(message) {
  Actions(
    /// Clean up an effect by a `Key` that has been `register`ed. This is used by
    /// effects to cancel or stop other effects. For example, a `set_interval`
    /// effect may register a cleanup function to cancel the timeout and then store
    /// the `Key` in the model for later use.
    cleanup: fn(Key) -> Nil,
    /// Dispatch a message to the runtime. Effects use this action to communicate
    /// back with the host app.
    dispatch: fn(message) -> Nil,
    /// Emit an event on the root element of the application. This is a DOM event
    /// that can be listened to like any other. The `Json` payload provided can be
    /// accessed under the event's `detail` property.
    emit: fn(String, Json) -> Nil,
    /// Provide a context value down to children that have requested it.
    provide: fn(String, Json) -> Nil,
    /// Register a cleanup function associated with a `Key`. If a cleanup function
    /// is already registered, it will be composed with the new one.
    register: fn(Key, fn() -> Nil) -> Nil,
    /// Access the root element of the application. For client apps this is the
    /// DOM node the app was mounted on to, for client components this is the
    /// component's shadow root, and for server components this should return `Nil`.
    root: fn() -> Dynamic,
    /// Useful only for server components running on the Erlang target: update the
    /// actor's selector so that it can receive messages from new sources.
    select: fn(Selector(message)) -> Nil,
  )
}

// CONSTANTS -------------------------------------------------------------------

pub const none: Effect(message) = None

const synchronous_kind: Kind = Synchronous

const before_paint_kind: Kind = BeforePaint

const after_paint_kind: Kind = AfterPaint

// CONSTRUCTORS ----------------------------------------------------------------

pub fn synchronous(run: fn(Actions(message)) -> Nil) -> Effect(message) {
  Effect(run:, kind: synchronous_kind)
}

pub fn before_paint(run: fn(Actions(message)) -> Nil) -> Effect(message) {
  Effect(run:, kind: before_paint_kind)
}

pub fn after_paint(run: fn(Actions(message)) -> Nil) -> Effect(message) {
  Effect(run:, kind: after_paint_kind)
}

pub fn batch(effects: List(Effect(message))) -> Effect(message) {
  case effects {
    [] | [None] -> none
    [one] | [one, None] | [None, one] -> one
    all -> Batch(all:)
  }
}

pub fn batch2(a: Effect(message), b: Effect(message)) -> Effect(message) {
  case a, b {
    None, None -> none
    None, _ -> b
    _, None -> a
    _, _ -> Batch(all: [a, b])
  }
}

pub fn deferred(run: fn(Actions(message)) -> Effect(message)) -> Effect(message) {
  Deferred(run:)
}

@external(erlang, "effect_ffi", "make_key")
@external(javascript, "./effect.ffi.mjs", "makeKey")
pub fn key() -> Key

//

///
///
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  case effect {
    None -> None
    // TODO: there's not really any need to eagerly apply the mapping function here.
    // We should do what we do for vnodes and sprinkle some crimes so mapping is
    // only performed as we traverse the effect "tree" during execution.
    Batch(all:) -> Batch(all: list.map(all, map(_, f)))
    Effect(run:, kind:) ->
      Effect(run: fn(actions) { run(do_comap_actions(actions, f)) }, kind:)
    Deferred(run:) ->
      Deferred(run: fn(actions) { run(do_comap_actions(actions, f)) |> map(f) })
  }
}

fn do_comap_actions(actions: Actions(a), f: fn(b) -> a) -> Actions(b) {
  Actions(
    cleanup: actions.cleanup,
    dispatch: fn(message) { actions.dispatch(f(message)) },
    emit: actions.emit,
    provide: actions.provide,
    register: actions.register,
    root: actions.root,
    select: fn(selector) { do_comap_select(actions, selector, f) },
  )
}

@target(erlang)
fn do_comap_select(
  actions: Actions(a),
  selector: Selector(b),
  f: fn(b) -> a,
) -> Nil {
  actions.select(process.map_selector(selector, f))
}

@target(javascript)
fn do_comap_select(_, _, _) -> Nil {
  Nil
}

//

///
///
pub fn perform(
  effect: Effect(message),
  actions: Actions(message),
) -> #(Effect(message), Effect(message)) {
  case effect {
    None -> #(none, none)
    Batch(all:) -> {
      let #(before_paint, after_paint) = do_perform(all, actions, [], [])

      #(batch(before_paint), batch(after_paint))
    }
    Effect(kind: BeforePaint, run:) -> #(synchronous(run), none)
    Effect(kind: AfterPaint, run:) -> #(none, synchronous(run))
    Effect(kind: Synchronous, run:) -> {
      run(actions)
      #(none, none)
    }
    Deferred(run:) -> perform(run(actions), actions)
  }
}

fn do_perform(
  effects: List(Effect(message)),
  actions: Actions(message),
  before_paint: List(Effect(message)),
  after_paint: List(Effect(message)),
) -> #(List(Effect(message)), List(Effect(message))) {
  case effects {
    [] -> #(before_paint, after_paint)

    [None, ..rest] -> do_perform(rest, actions, before_paint, after_paint)

    [Batch(all:), ..rest] -> {
      let #(before_paint, after_paint) =
        do_perform(all, actions, before_paint, after_paint)

      do_perform(rest, actions, before_paint, after_paint)
    }

    [Effect(kind: BeforePaint, run:), ..rest] ->
      do_perform(rest, actions, [synchronous(run), ..before_paint], after_paint)

    [Effect(kind: AfterPaint, run:), ..rest] ->
      do_perform(rest, actions, before_paint, [synchronous(run), ..after_paint])

    [Effect(kind: Synchronous, run:), ..rest] -> {
      run(actions)
      do_perform(rest, actions, before_paint, after_paint)
    }

    [Deferred(run:), ..rest] ->
      do_perform([run(actions), ..rest], actions, before_paint, after_paint)
  }
}
