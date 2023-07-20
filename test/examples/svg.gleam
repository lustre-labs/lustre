// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/attribute.{attribute}
import lustre/element.{Element, text}
import lustre/html.{button, div, p, svg}
import lustre/svg.{path}
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // A `simple` lustre application doesn't produce `Effect`s. These are best to 
  // start with if you're just getting started with lustre or you know you don't
  // need the runtime to manage any side effects.
  let app = lustre.simple(init, update, render)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]")
}

// MODEL -----------------------------------------------------------------------

pub type Model =
  Int

pub fn init() -> Model {
  0
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
  Reset
}

pub fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
    Reset -> 0
  }
}

// VIEW ------------------------------------------------------------------------

pub fn render(model: Model) -> Element(Msg) {
  div(
    [],
    [
      button(
        [event.on_click(Incr)],
        [plus([attribute.style([#("color", "red")])])],
      ),
      button([event.on_click(Decr)], [minus([])]),
      button([event.on_click(Reset)], [text("Reset")]),
      p([], [text(int.to_string(model))]),
    ],
  )
}

fn plus(attrs) {
  svg(
    [
      attribute("width", "15"),
      attribute("height", "15"),
      attribute("viewBox", "0 0 15 15"),
      attribute("fill", "none"),
      ..attrs
    ],
    [
      path([
        attribute(
          "d",
          "M8 2.75C8 2.47386 7.77614 2.25 7.5 2.25C7.22386 2.25 7 2.47386 7 2.75V7H2.75C2.47386 7 2.25 7.22386 2.25 7.5C2.25 7.77614 2.47386 8 2.75 8H7V12.25C7 12.5261 7.22386 12.75 7.5 12.75C7.77614 12.75 8 12.5261 8 12.25V8H12.25C12.5261 8 12.75 7.77614 12.75 7.5C12.75 7.22386 12.5261 7 12.25 7H8V2.75Z",
        ),
        attribute("fill", "currentColor"),
        attribute("fill-rule", "evenodd"),
        attribute("clip-rule", "evenodd"),
      ]),
    ],
  )
}

fn minus(attrs) {
  svg(
    [
      attribute("width", "15"),
      attribute("height", "15"),
      attribute("viewBox", "0 0 15 15"),
      attribute("fill", "none"),
      ..attrs
    ],
    [
      path([
        attribute(
          "d",
          "M2.25 7.5C2.25 7.22386 2.47386 7 2.75 7H12.25C12.5261 7 12.75 7.22386 12.75 7.5C12.75 7.77614 12.5261 8 12.25 8H2.75C2.47386 8 2.25 7.77614 2.25 7.5Z",
        ),
        attribute("fill", "currentColor"),
        attribute("fill-rule", "evenodd"),
        attribute("clip-rule", "evenodd"),
      ]),
    ],
  )
}
