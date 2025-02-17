import birdie
import envoy
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleamy/bench
import lustre/element.{type Element}
import lustre/element/html
import lustre/runtime/vdom

pub fn benchmark_10_rows_test() {
  use <- bool.guard(envoy.get("BENCHMARK") == Error(Nil), Nil)
  let rows = 10

  bench.run(
    [
      bench.Input("10 rows                   ", table_diff(rows, False, False)),
      bench.Input("10 rows (shuffled)        ", table_diff(rows, True, False)),
      bench.Input("10 rows (keyed)           ", table_diff(rows, False, True)),
      bench.Input("10 rows (keyed, shuffled) ", table_diff(rows, True, True)),
    ],
    [bench.Function("vdom.diff()", run_diff)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> birdie.snap("[benchmark] 10 table rows")
}

pub fn benchmark_100_rows_test() {
  use <- bool.guard(envoy.get("BENCHMARK") == Error(Nil), Nil)
  let rows = 100

  bench.run(
    [
      bench.Input("100 rows                   ", table_diff(rows, False, False)),
      bench.Input("100 rows (shuffled)        ", table_diff(rows, True, False)),
      bench.Input("100 rows (keyed)           ", table_diff(rows, False, True)),
      bench.Input("100 rows (keyed, shuffled) ", table_diff(rows, True, True)),
    ],
    [bench.Function("vdom.diff()", run_diff)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> birdie.snap("[benchmark] 100 table rows")
}

pub fn benchmark_1000_rows_test() {
  use <- bool.guard(envoy.get("BENCHMARK") == Error(Nil), Nil)
  let rows = 1000

  bench.run(
    [
      bench.Input(
        "1000 rows                   ",
        table_diff(rows, False, False),
      ),
      bench.Input("1000 rows (shuffled)        ", table_diff(rows, True, False)),
      bench.Input("1000 rows (keyed)           ", table_diff(rows, False, True)),
      bench.Input("1000 rows (keyed, shuffled) ", table_diff(rows, True, True)),
    ],
    [bench.Function("vdom.diff()", run_diff)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> birdie.snap("[benchmark] 1000 table rows")
}

pub fn benchmark_10_000_rows_test() {
  use <- bool.guard(envoy.get("BENCHMARK") == Error(Nil), Nil)
  let rows = 10_000

  bench.run(
    [
      bench.Input(
        "10,000 rows                   ",
        table_diff(rows, False, False),
      ),
      bench.Input(
        "10,000 rows (shuffled)        ",
        table_diff(rows, True, False),
      ),
      bench.Input(
        "10,000 rows (keyed)           ",
        table_diff(rows, False, True),
      ),
      bench.Input(
        "10,000 rows (keyed, shuffled) ",
        table_diff(rows, True, True),
      ),
    ],
    [bench.Function("vdom.diff()", run_diff)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> birdie.snap("[benchmark] 10000 table rows")
}

//

fn run_diff(input: #(Element(msg), Element(msg))) {
  vdom.diff(input.0, input.1, dict.new())
}

fn table_diff(rows: Int, shuffle: Bool, keyed: Bool) {
  let prev = view_table(rows, 0, shuffle, keyed)
  let next = view_table(rows, rows / 2, shuffle, keyed)

  #(prev, next)
}

// VIEW ------------------------------------------------------------------------

fn view_table(
  rows: Int,
  offset: Int,
  shuffle: Bool,
  keyed: Bool,
) -> Element(msg) {
  let rows = list.range(1 + offset, rows + offset)
  let rows = case shuffle {
    True -> list.shuffle(rows)
    False -> rows
  }

  case keyed {
    True -> view_keyed_table(rows)
    False -> view_unkeyed_table(rows)
  }
}

fn view_keyed_table(rows: List(Int)) -> Element(msg) {
  element.keyed(html.table([], _), {
    use id, pos <- list.index_map(rows)
    let key = int.to_string(id)

    #(key, view_row(id, pos))
  })
}

fn view_unkeyed_table(rows: List(Int)) -> Element(msg) {
  html.table([], {
    use id, pos <- list.index_map(rows)

    view_row(id, pos)
  })
}

fn view_row(id: Int, pos: Int) -> Element(msg) {
  html.tr([], [
    html.td([], [html.text(int.to_string(pos))]),
    html.td([], [html.text("Row id"), html.text(int.to_string(id))]),
    html.td([], [html.button([], [html.text("Delete")])]),
  ])
}
