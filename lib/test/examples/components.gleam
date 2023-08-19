// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/int
import gleam/list
import gleam/map
import gleam/option.{Some}
import gleam/result
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{element, text}
import lustre/element/html.{button, div, li, ol, p, slot}
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(_) =
    lustre.component(
      "custom-counter",
      counter_init,
      counter_update,
      counter_view,
      map.from_list([
        #(
          "count",
          fn(attr) {
            dynamic.int(attr)
            |> result.map(GotCount)
          },
        ),
      ]),
    )

  // A `simple` lustre application doesn't produce `Effect`s. These are best to 
  // start with if you're just getting started with lustre or you know you don't
  // need the runtime to manage any side effects.
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)

  Nil
}

fn init(_) {
  []
}

fn update(history, msg) {
  case msg {
    "reset" -> []
    _ -> [msg, ..history]
  }
}

fn view(history) {
  let on_custom_click = {
    use _ <- event.on("custom-click")
    Some("click")
  }
  div(
    [],
    [
      button([event.on_click("reset")], [text("Reset")]),
      ol([], list.map(history, fn(msg) { li([], [text(msg)]) })),
      element(
        "custom-counter",
        [on_custom_click, attribute.property("count", list.length(history))],
        [ol([], list.map(history, fn(msg) { li([], [text(msg)]) }))],
      ),
    ],
  )
}

// COUNTER ---------------------------------------------------------------------

fn counter_init() {
  #(0, effect.none())
}

type CounterMsg {
  GotCount(Int)
  Clicked
}

fn counter_update(count, msg) {
  case msg {
    GotCount(count) -> #(count, effect.none())
    Clicked -> #(count, event.emit("custom-click", Nil))
  }
}

fn counter_view(count) {
  div(
    [],
    [
      button([event.on_click(Clicked)], [text("Click me!")]),
      p([], [text("Count: "), text(int.to_string(count))]),
      slot([]),
    ],
  )
}
