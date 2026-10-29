// IMPORTS ---------------------------------------------------------------------

import counter
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)

  // When starting a Lustre app, you can pass in initial data as "flags" to your
  // application. Because the pieces of your app are supposed to be *pure*, flags
  // are a good opportunity to pass in initial data from side effects such as
  // randomness or HTTP requests that you want to have immediately available.
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

/// For this example, our main "app" doesn't need any state or functionality of
/// its own. Instead it manages the state of another gleam module, delegating 
/// init, update and view functions to the other module. This approach
/// allows for separating state among modules without needing full web
/// components. It demonstrates how to make an Element(OtherMsg) returned from
/// the other module's view callback compatible in a generic way with a parent 
/// module using element.map
type Model {
  Model(counter: counter.Model)
}

fn init(_) -> Model {
  Model(counter: counter.init(0))
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  CounterUpdated(counter.Msg)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    CounterUpdated(counter_msg) ->
      Model(counter: counter.update(model.counter, counter_msg))
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("p-32 mx-auto w-full max-w-2xl")], [
    // counter.view returns an Element parameterised on the Msg type declared 
    // in the counter module. element.map/1 here makes it compatible with our
    // Msg type
    counter.view(model.counter) |> element.map(CounterUpdated),
  ])
}
