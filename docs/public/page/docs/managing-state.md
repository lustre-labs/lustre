# Managing state

We saw in the quickstart guide that Lustre applications are built using the
Model-View-Update architecture. For folks used to building with React or most
other frontend frameworks, it can be a bit of a shock to work without access to
local component state.

In this guide we'll look at how to manage state in a variety of scenarios
_without_ using local component state. It's important to get a solid grasp on
this _before_ looking at Lustre's approach to components because they're built on
the same principles!

## Semi-encapsulated components

Before reaching for Lustre's stateful components, you might consider a
semi-encapsulated approach. This is where you have a separate Gleam module that
defines it's own `Model`, `init`, `Msg`, and `update` (and optionally a `view`
too) but still manage things from your top-level application.

For example, we may define a `counter` module:

```gleam
// app/counter.gleam

import gleam/int
import lustre/element.{Element}
import lustre/element/html
import lustre/event

pub opaque type Model {
  Model(Int)
}

pub fn init() -> Model {
  Model(0)
}

pub type Msg {
  Incr
  Decr
  Double
  Reset
}

pub fn update(model: Model, msg: Msg) -> Model {
  let Model(count) = model
  case msg {
    Incr -> Model(count + 1)
    Decr -> Model(count - 1)
    Double -> Model(count * 2)
    Reset -> Model(0)
  }
}

pub fn view(model: Model) -> Element(Msg) {
  let Model(count) = model
  let count = int.to_string(count)

  html.div([], [
    html.p([], [element.text(count)]),
    html.button([event.on_click(Decr)], [html.text("-")]),
    html.button([event.on_click(Incr)], [html.text("+")]),
    html.button([event.on_click(Double)], [html.text("x2")]),
    html.button([event.on_click(Reset)], [html.text("Reset")]),
  ])
}
```

Now we can create and manage multiple counters in our main application:

```gleam
// app.gleam

import app/counter
import lustre
import lustre/element.{Element}
import lustre/element/html

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)
}

pub type Model {
  Model(
    // Our model will hold two separate counters, each with their own independent
    // state.
    counter1: counter.Model,
    counter2: counter.Model,
  )
}

pub fn init() -> Model {
  Model(
    counter.init(),
    counter.init(),
  )
}

pub type Msg {
  //
  Counter1(counter.Msg)
  Counter2(counter.Msg)
}

pub fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Counter1(msg) -> Model(..model,
      counter1: counter.update(model.counter1, msg)
    )

    Counter2(msg) -> Model(..model,
      counter2: counter.update(model.counter2, msg)
    )
  }
}

pub fn view(model: Model) -> Element(Msg) {
  let Model(counter1, counter2) = model

  html.div([], [
    counter.view(counter1) |> element.map(Counter1),
    counter.view(counter2) |> element.map(Counter2),
  ])
}
```

Note that we're using [`element.map`](/api/lustre/element#map) to map the events
from each counter view to a `Msg` type our application understands! In Lustre,
the [`Element`](/api/lustre/element#element-type) type is parameterised by the
type of messages they can emit. This is how Lustre achieves type-safe event handling.

This approach can get quite sophisticated. For example you may want to make your
component's `Model` type opaque and optionally provide some helper functions to
extract any data parents may need to know about. You might also choose to split
your component's `Msg` type and keep a separate `InternalMsg` type that can't
be constructed outside of the module.

Taking the counter example from above, perhaps we want parents to only be able to
reset the counter and query the current count, but all other messages are handled
internally:

```gleam
pub type Msg {
  Reset
  Internal(InternalMsg)
}

pub opaque type InternalMsg {
  Incr
  Decr
  Double
}

pub fn count(model: Model) -> Int {
  let Model(count) = model
  count
}
```

The parent could still have a button to reset all counters back to `0`, but it
wouldn't be able to mess with the internal state in any other way.

After a while you may you find your semi-encapsulated components have a lot of
internal state or many messages that are only relevant to that component. If that
happens, it may be time to consider a [stateful component](/docs/components)
instead.

## Separating page state

```gleam
type Model {

}
```

## Preserving state across page changes

```gleam
type Model = Map(String, PageModel)

type PageModel {

}
```

## Sharing state between pages

```gleam
import gleam/map.{Map}

type Model {
  Model(
    shared: SharedModel,
    pages: Map(String, PageModel)
  )
}

type SharedModel {
  SharedModel()
}

type PageModel {

}
```
