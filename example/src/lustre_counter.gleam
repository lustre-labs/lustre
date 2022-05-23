import gleam/int
import lustre
import lustre/element.{button, div, p, span, text}
import lustre/event.{dispatch, on_click}
import lustre/cmd
import gleam/map.{Map}
import gleam/list
import gleam/option

pub fn main() {
  let app = lustre.application(#(init_state(), cmd.none()), update, render)
  lustre.start(app, "#app")
}

type State {
  State(ctr: Int, counters: Map(Int, Int))
}

fn init_state() {
  State(ctr: 2, counters: map.from_list([#(1, 0)]))
}

pub type Action {
  Add
  Remove(id: Int)
  Increment(id: Int)
  Decrement(id: Int)
}

fn update(state, action) {
  case action {
    Add -> #(
      State(
        ..state,
        ctr: state.ctr + 1,
        counters: state.counters
        |> map.insert(state.ctr, 0),
      ),
      cmd.none(),
    )
    Remove(id) -> #(
      State(
        ..state,
        counters: state.counters
        |> map.delete(id),
      ),
      cmd.none(),
    )
    Increment(id) -> #(
      State(
        ..state,
        counters: state.counters
        |> map.update(id, fn(opt_ctr) { option.unwrap(opt_ctr, 0) + 1 }),
      ),
      cmd.none(),
    )
    Decrement(id) -> #(
      State(
        ..state,
        counters: state.counters
        |> map.update(id, fn(opt_ctr) { option.unwrap(opt_ctr, 0) - 1 }),
      ),
      cmd.none(),
    )
  }
}

fn render(state) {
  let render_counter = fn(pair) {
    let #(id, value) = pair
    p(
      [],
      [
        button([on_click(dispatch(Decrement(id)))], [text("-")]),
        span([], [text(" "), text(int.to_string(value)), text(" ")]),
        button([on_click(dispatch(Increment(id)))], [text("+")]),
        span([], [text(" ")]),
        button([on_click(dispatch(Remove(id)))], [text("remove")]),
      ],
    )
  }

  div(
    [],
    [
      div(
        [],
        state.counters
        |> map.to_list
        |> list.map(render_counter),
      ),
      p([], [button([on_click(dispatch(Add))], [text("add")])]),
    ],
  )
}
