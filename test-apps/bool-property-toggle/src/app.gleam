import lustre
import lustre/attribute
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event

pub type Model =
  Nil

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  #(Nil, effect.none())
}

pub type Msg {
  UserPressedButton
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserPressedButton -> {
      #(model, effect.none())
    }
  }
}

pub fn view(_model: Model) -> element.Element(Msg) {
  html.div([], [
    html.button([event.on_click(UserPressedButton)], [
      element.text("raise event"),
    ]),
    html.div([], [
      html.button([attribute.disabled(True)], [
        element.text("I should always be disabled"),
      ]),
    ]),
  ])
}

pub fn main() {
  let app = lustre.application(init, update, view)

  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}
