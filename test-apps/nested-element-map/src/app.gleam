import lustre
import lustre/element.{type Element, element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model =
  String

fn init(_flags) -> Model {
  "div"
}

// UPDATE ----------------------------------------------------------------------
pub opaque type Baby {
  Waaa
}

pub opaque type Child {
  BabyMsg(Baby)
}

pub opaque type Msg {
  ChildMsg(Child)
}

fn update(model: Model, _msg: Msg) -> Model {
  model
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.div([], [
    html.text("parent"),
    html.div([], [
      html.text("child"),
      html.div([event.on_click(Waaa)], [html.text("baby")])
        |> element.map(BabyMsg),
    ])
      |> element.map(ChildMsg),
  ])
}
