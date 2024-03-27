import gleam/uri.{type Uri}
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import modem
// These examples are written with `lustre/ui` in mind. They'll work regardless,
// but to see what `lustre/ui` can do make sure to run each of these examples with
// the `--use-example-styles` flag:
//
//   $ gleam run -m lustre/dev start --use-example-styles
//
// In your own apps, make sure to add the `lustre/ui` dependency and include the
// stylesheet somewhere.
import lustre/ui
import lustre/ui/aside

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(Model, modem.init(on_route_change))
}

fn on_route_change(uri: Uri) -> Msg {
  let path = uri.path_segments(uri.path)

  RouterPushedPath(path)
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  RouterPushedPath(List(String))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  todo
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  todo
}
