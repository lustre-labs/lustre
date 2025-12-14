@target(javascript)
import booklet
@target(javascript)
import gleam/int
@target(javascript)
import gleam/list
@target(javascript)
import gleam/string
@target(javascript)
import lustre
@target(javascript)
import lustre/attribute
@target(javascript)
import lustre/element.{type Element}
@target(javascript)
import lustre/element/html
@target(javascript)
import lustre/element/keyed
@target(javascript)
import lustre/event
@target(javascript)
import lustre_test

// TYPES -----------------------------------------------------------------------

@target(javascript)
pub type Runtime(msg, model)

@target(javascript)
type CounterMsg {
  Increment
  Decrement
  Reset
  SetTo(Int)
}

// TESTS -----------------------------------------------------------------------

@target(javascript)
pub fn client_runtime_map_with_events_test() {
  use <- lustre_test.test_filter("client_runtime_map_with_events_test")

  // Render initial vdom to HTML string
  let initial =
    html.div([], [
      element.map(
        html.button([event.on_click(5)], [html.text("Click")]),
        int.add(1, _),
      ),
    ])

  let html_string = element.to_string(initial)

  let app =
    lustre.simple(fn(_) { [] }, fn(model, msg) { [msg, ..model] }, fn(_model) {
      initial
    })

  use runtime <- with_client_runtime(html_string, app)

  // Click the button
  use <- emit("button", "click")

  let model = get_model(runtime)
  assert model == [6]
}

@target(javascript)
pub fn client_runtime_memo_caching_test() {
  use <- lustre_test.test_filter("client_runtime_memo_caching_test")

  // Create vdom with counter
  let call_count = booklet.new(0)
  let view = fn(model) {
    html.div([], [
      element.memo([], fn() {
        booklet.update(call_count, int.add(1, _))
        html.h1([], [html.text("Memoized " <> int.to_string(model))])
      }),
    ])
  }

  let initial = view(0)
  let html_string = element.to_string(initial)

  // called once for element.to_string
  assert booklet.get(call_count) == 1

  let app = lustre.simple(fn(_) { 0 }, fn(model, _msg) { model + 1 }, view)

  use runtime <- with_client_runtime(html_string, app)

  // called again to diff against the initial vdom
  assert booklet.get(call_count) == 2

  use <- send(runtime, Nil)
  use <- send(runtime, Nil)

  assert get_model(runtime) == 2
  assert booklet.get(call_count) == 2

  // Verify that the DOM hasn't changed.
  assert lustre_test.nodes_equal_ignoring_memo(get_vdom(), initial)
}

@target(javascript)
pub fn client_runtime_memo_events_test() {
  use <- lustre_test.test_filter("client_runtime_memo_events_test")

  let view = fn(_model) {
    html.div([], [
      element.memo([], fn() {
        html.button([event.on_click(1)], [html.text("Increment")])
      }),
    ])
  }

  let initial = view(0)
  let html_string = element.to_string(initial)

  let app = lustre.simple(fn(_) { 0 }, int.add, view)

  use runtime <- with_client_runtime(html_string, app)

  use <- emit("button", "click")
  assert get_model(runtime) == 1

  use <- emit("button", "click")
  assert get_model(runtime) == 2

  use <- emit("button", "click")
  assert get_model(runtime) == 3
}

// COMPREHENSIVE RUNTIME TESTS -------------------------------------------------

@target(javascript)
pub fn client_runtime_single_event_test() {
  use <- lustre_test.test_filter("client_runtime_single_event_test")

  let view = fn(model) {
    html.div([], [
      html.button([event.on_click(Decrement)], [html.text("-")]),
      html.p([], [html.text(int.to_string(model))]),
      html.button([event.on_click(Increment)], [html.text("+")]),
    ])
  }

  let initial = view(0)
  let html_string = element.to_string(initial)

  let app =
    lustre.simple(
      fn(_) { 0 },
      fn(model, msg) {
        case msg {
          Increment -> model + 1
          Decrement -> model - 1
          Reset -> 0
          SetTo(n) -> n
        }
      },
      view,
    )

  use runtime <- with_client_runtime(html_string, app)

  // Click increment button
  use <- emit("button:nth-child(3)", "click")

  assert get_model(runtime) == 1
}

@target(javascript)
pub fn client_runtime_multiple_events_test() {
  use <- lustre_test.test_filter("client_runtime_multiple_events_test")

  let view = fn(model) {
    html.div([], [
      html.button([event.on_click(Decrement)], [html.text("-")]),
      html.p([], [html.text(int.to_string(model))]),
      html.button([event.on_click(Increment)], [html.text("+")]),
    ])
  }

  let initial = view(0)
  let html_string = element.to_string(initial)

  let app =
    lustre.simple(
      fn(_) { 0 },
      fn(model, msg) {
        case msg {
          Increment -> model + 1
          Decrement -> model - 1
          Reset -> 0
          SetTo(n) -> n
        }
      },
      view,
    )

  use runtime <- with_client_runtime(html_string, app)

  // Click increment three times
  use <- emit("button:nth-child(3)", "click")
  use <- emit("button:nth-child(3)", "click")
  use <- emit("button:nth-child(3)", "click")

  assert get_model(runtime) == 3
}

@target(javascript)
pub fn client_runtime_fragment_rendering_test() {
  use <- lustre_test.test_filter("client_runtime_fragment_rendering_test")

  let view = fn(model) {
    html.div([], [
      element.fragment([
        html.p([], [html.text("Count: ")]),
        html.p([], [html.text(int.to_string(model))]),
      ]),
    ])
  }

  let initial = view(0)
  let html_string = element.to_string(initial)

  let app =
    lustre.simple(
      fn(_) { 0 },
      fn(model, msg) {
        case msg {
          Increment -> model + 1
          Decrement -> model - 1
          Reset -> 0
          SetTo(n) -> n
        }
      },
      view,
    )

  use runtime <- with_client_runtime(html_string, app)

  use <- send(runtime, SetTo(5))

  let html = get_html()
  assert string.contains(html, "Count: ")
  assert string.contains(html, "5")
}

// FORM INPUT TESTS ------------------------------------------------------------

@target(javascript)
pub fn client_runtime_controlled_text_input_test() {
  use <- lustre_test.test_filter("client_runtime_controlled_text_input_test")

  let view = fn(model) {
    html.div([], [
      html.input([
        attribute.type_("text"),
        attribute.value(model),
        event.on_input(fn(value) { value }),
      ]),
      html.p([], [html.text("Value: " <> model)]),
    ])
  }

  let app = lustre.simple(fn(_) { "" }, fn(_model, msg) { msg }, view)

  let initial = view("")
  let html_string = element.to_string(initial)

  use runtime <- with_client_runtime(html_string, app)

  // Type into input
  use <- emit_with_value("input", "input", "hello")
  assert get_model(runtime) == "hello"

  use <- emit_with_value("input", "input", "hello world")
  assert get_model(runtime) == "hello world"
}

@target(javascript)
pub fn client_runtime_controlled_checkbox_test() {
  use <- lustre_test.test_filter("client_runtime_controlled_checkbox_test")

  let view = fn(model) {
    html.div([], [
      html.input([
        attribute.type_("checkbox"),
        attribute.checked(model),
        event.on_check(fn(checked) { checked }),
      ]),
      html.p([], [html.text("Checked: " <> string.inspect(model))]),
    ])
  }

  let app = lustre.simple(fn(_) { False }, fn(_model, msg) { msg }, view)

  let initial = view(False)
  let html_string = element.to_string(initial)

  use runtime <- with_client_runtime(html_string, app)

  // Click checkbox to check it
  use <- emit_with_value("input[type=checkbox]", "change", "true")
  assert get_model(runtime) == True

  // Click checkbox to uncheck it
  use <- emit_with_value("input[type=checkbox]", "change", "false")
  assert get_model(runtime) == False
}

@target(javascript)
pub fn client_runtime_select_dropdown_test() {
  use <- lustre_test.test_filter("client_runtime_select_dropdown_test")

  let view = fn(model) {
    html.div([], [
      html.select([event.on_input(fn(value) { value })], [
        html.option([attribute.value("red")], "Red"),
        html.option([attribute.value("green")], "Green"),
        html.option([attribute.value("blue")], "Blue"),
      ]),
      html.p([], [html.text("Selected: " <> model)]),
    ])
  }

  let app = lustre.simple(fn(_) { "red" }, fn(_model, msg) { msg }, view)

  let initial = view("red")
  let html_string = element.to_string(initial)

  use runtime <- with_client_runtime(html_string, app)

  // Select different option
  use <- emit_with_value("select", "input", "green")
  assert get_model(runtime) == "green"

  use <- emit_with_value("select", "input", "blue")
  assert get_model(runtime) == "blue"
}

// MEMO DEPENDENCY TESTS -------------------------------------------------------

@target(javascript)
pub fn client_runtime_memo_dependency_change_test() {
  use <- lustre_test.test_filter("client_runtime_memo_dependency_change_test")

  let call_count = booklet.new(0)

  let view = fn(model: Int) {
    html.div([], [
      element.memo([element.ref(model)], fn() {
        booklet.update(call_count, int.add(1, _))
        html.p([], [html.text("Count: " <> int.to_string(model))])
      }),
      html.button([event.on_click(0)], [html.text("Increment")]),
    ])
  }

  let app = lustre.simple(fn(_) { 0 }, fn(model, _msg) { model + 1 }, view)

  let initial = view(0)
  let html_string = element.to_string(initial)

  // Called once for element.to_string
  assert booklet.get(call_count) == 1

  use _runtime <- with_client_runtime(html_string, app)

  // Called again during initial mount/diff
  assert booklet.get(call_count) == 2

  // Click to change dependency
  use <- emit("button", "click")
  assert booklet.get(call_count) == 3

  // Click again to change dependency
  use <- emit("button", "click")
  assert booklet.get(call_count) == 4
}

@target(javascript)
pub fn client_runtime_memo_stable_dependency_test() {
  use <- lustre_test.test_filter("client_runtime_memo_stable_dependency_test")

  let call_count = booklet.new(0)

  let view = fn(model: #(Int, String)) {
    let #(count, _text) = model
    html.div([], [
      element.memo([element.ref(count)], fn() {
        booklet.update(call_count, int.add(1, _))
        html.p([], [html.text("Count: " <> int.to_string(count))])
      }),
      html.button([event.on_click("update")], [html.text("Update Text")]),
    ])
  }

  let app =
    lustre.simple(
      fn(_) { #(0, "") },
      fn(model, _msg) {
        let #(count, text) = model
        #(count, text <> "x")
      },
      view,
    )

  let initial = view(#(0, ""))
  let html_string = element.to_string(initial)

  // Called once for element.to_string
  assert booklet.get(call_count) == 1

  use _runtime <- with_client_runtime(html_string, app)

  // Called again during initial mount/diff
  assert booklet.get(call_count) == 2

  // Click to update text (but not count - dependency is stable)
  use <- emit("button", "click")
  assert booklet.get(call_count) == 2

  // Click again
  use <- emit("button", "click")
  assert booklet.get(call_count) == 2
}

// EVENT BUBBLING TEST ---------------------------------------------------------

@target(javascript)
pub fn client_runtime_event_bubbling_test() {
  use <- lustre_test.test_filter("client_runtime_event_bubbling_test")

  let view = fn(model: List(String)) {
    html.div([event.on_click("parent")], [
      html.p([], [html.text("Last clicked: " <> string.inspect(model))]),
      html.button([event.on_click("child")], [html.text("Click Me")]),
    ])
  }

  let app = lustre.simple(fn(_) { [] }, fn(model, msg) { [msg, ..model] }, view)

  let initial = view([])
  let html_string = element.to_string(initial)

  use runtime <- with_client_runtime(html_string, app)

  // Click button - child element's handler should fire
  use <- emit("button", "click")
  assert get_model(runtime) == ["parent", "child"]

  // Click paragraph (only parent handler exists here)
  use <- emit("p", "click")
  assert get_model(runtime) == ["parent", "parent", "child"]
}

// NESTED FRAGMENTS TESTS ------------------------------------------------------

@target(javascript)
pub fn client_runtime_nested_fragments_test() {
  use <- lustre_test.test_filter("client_runtime_nested_fragments_test")

  let view = fn(model: Int) {
    html.div([], [
      element.fragment([
        html.h1([], [html.text("Outer")]),
        element.fragment([
          html.p([], [html.text("Inner: " <> int.to_string(model))]),
          html.button([event.on_click(0)], [html.text("Increment")]),
        ]),
      ]),
    ])
  }

  let app = lustre.simple(fn(_) { 0 }, fn(model, _msg) { model + 1 }, view)

  let initial = view(0)
  let html_string = element.to_string(initial)

  use runtime <- with_client_runtime(html_string, app)

  // Click button in nested fragment
  use <- emit("button", "click")
  assert get_model(runtime) == 1

  let html = get_html()
  assert string.contains(html, "Inner: 1")
}

@target(javascript)
pub fn client_runtime_keyed_fragments_test() {
  use <- lustre_test.test_filter("client_runtime_keyed_fragments_test")

  let view = fn(model: List(String)) {
    html.div([], [
      keyed.fragment(
        list.map(model, fn(item) { #(item, html.p([], [html.text(item)])) }),
      ),
      html.button([event.on_click(list.reverse(model))], [
        html.text("Reverse"),
      ]),
    ])
  }

  let initial_list = ["First", "Second", "Third"]
  let app = lustre.simple(fn(_) { initial_list }, fn(_model, msg) { msg }, view)

  let initial = view(initial_list)
  let html_string = element.to_string(initial)

  use runtime <- with_client_runtime(html_string, app)

  // Initial state should show items in order
  let html = get_html()
  let first_pos = case string.split(html, "First") {
    [before, ..] -> string.length(before)
    _ -> -1
  }
  let second_pos = case string.split(html, "Second") {
    [before, ..] -> string.length(before)
    _ -> -1
  }
  let third_pos = case string.split(html, "Third") {
    [before, ..] -> string.length(before)
    _ -> -1
  }
  assert first_pos < second_pos
  assert second_pos < third_pos

  // Click to reverse the list
  use <- emit("button", "click")
  assert get_model(runtime) == ["Third", "Second", "First"]

  let html = get_html()
  let first_pos = case string.split(html, "First") {
    [before, ..] -> string.length(before)
    _ -> -1
  }
  let second_pos = case string.split(html, "Second") {
    [before, ..] -> string.length(before)
    _ -> -1
  }
  let third_pos = case string.split(html, "Third") {
    [before, ..] -> string.length(before)
    _ -> -1
  }
  assert third_pos < second_pos
  assert second_pos < first_pos
}

// FFI ------------------------------------------------------------------------

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "with_client_runtime")
pub fn with_client_runtime(
  initial_html: String,
  app: lustre.App(Nil, model, msg),
  test_callback: fn(Runtime(msg, model)) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "send")
pub fn send(
  runtime: Runtime(msg, model),
  msg: msg,
  callback: fn() -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "emit")
pub fn emit(selector: String, event_name: String, callback: fn() -> Nil) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "emit_with_value")
pub fn emit_with_value(
  selector: String,
  event_name: String,
  value: String,
  callback: fn() -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "get_html")
pub fn get_html() -> String

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "get_vdom")
pub fn get_vdom() -> Element(msg)

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "model")
pub fn get_model(runtime: Runtime(msg, model)) -> model
