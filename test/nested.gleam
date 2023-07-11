// IMPORTS ---------------------------------------------------------------------

import counter
import gleam/list
import gleam/map.{Map}
import lustre
import lustre/element.{Element, div}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // A `simple` lustre application doesn't produce `Cmd`s. These are best to 
  // start with if you're just getting started with lustre or you know you don't
  // need the runtime to manage any side effects.
  let app = lustre.simple(init, update, render)
  let assert Ok(dispatch) = lustre.start(app, "body")

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Map(Int, counter.Model)

fn init() -> Model {
  use counters, id <- list.fold(list.range(1, 10), map.new())

  map.insert(counters, id, counter.init())
}

// UPDATE ----------------------------------------------------------------------

type Msg =
  #(Int, counter.Msg)

fn update(model: Model, msg: Msg) -> Model {
  let #(id, counter_msg) = msg
  let assert Ok(counter) = map.get(model, id)

  map.insert(model, id, counter.update(counter, counter_msg))
}

// RENDER ----------------------------------------------------------------------

fn render(model: Model) -> Element(Msg) {
  div(
    [],
    {
      use #(id, counter) <- list.map(map.to_list(model))
      let tagger = fn(msg) { #(id, msg) }

      element.map(counter.render(counter), tagger)
    },
  )
}
