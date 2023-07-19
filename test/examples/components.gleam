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
import lustre/element.{h, t}
import lustre/event
import lustre/html.{button, div, li, ol, p}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(_) =
    lustre.component(
      "custom-counter",
      counter_init,
      counter_update,
      counter_render,
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
  let app = lustre.simple(init, update, render)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]")

  Nil
}

fn init() {
  []
}

fn update(history, msg) {
  [msg, ..history]
}

fn render(history) {
  let on_custom_click = {
    use _ <- event.on("custom-click")
    Some("click")
  }
  div(
    [],
    [
      ol([], list.map(history, fn(msg) { li([], [t(msg)]) })),
      h(
        "custom-counter",
        [on_custom_click, attribute.property("count", list.length(history))],
        [],
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

fn counter_render(count) {
  div(
    [],
    [
      button([event.on_click(Clicked)], [t("Click me!")]),
      p([], [t("Count: "), t(int.to_string(count))]),
    ],
  )
}
