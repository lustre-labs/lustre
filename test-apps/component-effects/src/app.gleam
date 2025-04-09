// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/list
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.component(init, update, view, [])
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(_) -> #(Model, Effect(Msg)) {
  #(0, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  DispatchOneThousandMessages(should_flush: Bool)
  Flush
  AddOne
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    AddOne -> #(model + 1, effect.none())
    DispatchOneThousandMessages(should_flush:) -> #(
      model,
      effect.batch([
        alert_before_paint(model, should_flush),
        dispatch_one_thousand_messages(),
        alert_after_paint(model + 500, should_flush),
      ]),
    )
    Flush -> #(model - 500, effect.none())
  }
}

fn alert_before_paint(previous: Int, should_flush: Bool) -> Effect(Msg) {
  use dispatch, _ <- effect.before_paint
  let count = text_content("#count")
  let previous_count = int.to_string(previous)

  alert(
    "This is before the browser paints: the count in the DOM is "
    <> count
    <> " but you still see "
    <> previous_count
    <> ".",
  )

  case should_flush {
    True -> dispatch(Flush)
    False -> Nil
  }
}

fn dispatch_one_thousand_messages() -> Effect(Msg) {
  effect.batch(list.repeat(effect.from(fn(dispatch) { dispatch(AddOne) }), 1000))
}

fn alert_after_paint(new: Int, did_flush: Bool) -> Effect(Msg) {
  use _, _ <- effect.after_paint
  let new_count = int.to_string(new)
  let info = case did_flush {
    True ->
      "the count was reset to " <> new_count <> " before the browser painted"
    False -> "you can see the updated count!"
  }

  alert("This is after the browser paints; " <> info)
}

@external(javascript, "./app.ffi.mjs", "alert")
fn alert(message: String) -> Nil

@external(javascript, "./app.ffi.mjs", "text_content")
fn text_content(selector: String) -> String

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  alert("Calling the view function")

  html.div([], [
    html.button([event.on_click(DispatchOneThousandMessages(False))], [
      html.text("Dispatch 1000 messages"),
    ]),
    html.button([event.on_click(DispatchOneThousandMessages(True))], [
      html.text("Dispatch 1000 messages (flush)"),
    ]),
    html.p([attribute.id("count")], [
      html.text("The count is: "),
      html.text(int.to_string(model)),
    ]),
  ])
}
