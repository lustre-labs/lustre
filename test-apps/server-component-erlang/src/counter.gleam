import gleam/erlang/process
import gleam/int
import gleam/json
import lustre
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.application(init, update, view)
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(initial_count: Int) -> #(Model, Effect(Msg)) {
  let model = case initial_count < 0 {
    True -> 0
    False -> initial_count
  }

  #(model, tick())
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
  Tick
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Incr -> #(model + 1, effect.none)
    Decr -> #(model - 1, effect.none)
    Tick -> #(
      model + 1,
      effect.batch([tick(), event.emit("tick", json.int(model + 1))]),
    )
  }
}

fn tick() -> Effect(Msg) {
  use dispatch <- effect.from
  let _ =
    process.start(
      fn() {
        let self = process.new_subject()
        process.send_after(self, 1000, Tick)
        let msg = process.receive_forever(self)

        dispatch(msg)
      },
      True,
    )

  Nil
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model)

  html.div([], [
    html.button([event.on_click(Incr)], [html.text("+")]),
    html.slot([]),
    html.p([], [html.text(count)]),
    html.button([event.on_click(Decr)], [html.text("-")]),
  ])
}
