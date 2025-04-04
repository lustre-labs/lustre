import lustre
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

//

type Model {
  Model(throttled: String, debounced: String)
}

fn init(_) -> Model {
  Model(throttled: "", debounced: "")
}

type Msg {
  UserTypedThrottledMessage(String)
  UserTypedDebouncedMessage(String)
}

fn update(model: Model, msg: Msg) -> Model {
  case echo msg {
    UserTypedThrottledMessage(text) -> Model(..model, throttled: text)
    UserTypedDebouncedMessage(text) -> Model(..model, debounced: text)
  }
}

fn view(model: Model) -> Element(Msg) {
  element.fragment([
    html.div([], [
      html.p([], [html.text("throttled:")]),
      html.input([
        attribute.value(model.throttled),
        event.on_input(UserTypedThrottledMessage) |> event.throttle(1000),
      ]),
      html.p([], [html.text(model.throttled)]),
    ]),
    html.div([], [
      html.p([], [html.text("debounced:")]),
      html.input([
        attribute.value(model.debounced),
        event.on_input(UserTypedDebouncedMessage) |> event.debounce(1000),
      ]),
      html.p([], [html.text(model.debounced)]),
    ]),
  ])
}
