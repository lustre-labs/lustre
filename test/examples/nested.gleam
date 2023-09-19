// IMPORTS ---------------------------------------------------------------------

import examples/counter
import gleam/list
import gleam/map.{Map}
import gleam/pair
import lustre
import lustre/element.{Element}
import lustre/element/html.{div}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // A `simple` lustre application doesn't produce `Effect`s. These are best to 
  // start with if you're just getting started with lustre or you know you don't
  // need the runtime to manage any side effects.
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Map(Int, counter.Model)

fn init(_) -> Model {
  use counters, id <- list.fold(list.range(1, 10), map.new())

  map.insert(counters, id, counter.init(Nil))
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

fn view(model: Model) -> Element(Msg) {
  let counters = {
    use rest, id, counter <- map.fold(model, [])
    let el = element.map(counter.view(counter), pair.new(id, _))

    [el, ..rest]
  }

  div([], counters)
}
