import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleamy/bench
import lustre/element.{type Element}
import lustre/element/html
import lustre/runtime/vdom

pub fn run() {
  bench.run(
    [
      bench.Input("     10 rows    ", table_diff(10, False, False)),
      bench.Input("     10 rows + s", table_diff(10, True, False)),
      bench.Input("     10 rows +k ", table_diff(10, False, True)),
      bench.Input("     10 rows +ks", table_diff(10, True, True)),
      //
      bench.Input("    100 rows    ", table_diff(100, False, False)),
      bench.Input("    100 rows + s", table_diff(100, True, False)),
      bench.Input("    100 rows +k ", table_diff(100, False, True)),
      bench.Input("    100 rows +ks", table_diff(100, True, True)),
      //
      bench.Input("  1,000 rows    ", table_diff(1000, False, False)),
      bench.Input("  1,000 rows + s", table_diff(1000, True, False)),
      bench.Input("  1,000 rows +k ", table_diff(1000, False, True)),
      bench.Input("  1,000 rows +ks", table_diff(1000, True, True)),
      //
      bench.Input(" 10,000 rows    ", table_diff(10_000, False, False)),
      bench.Input(" 10,000 rows + s", table_diff(10_000, True, False)),
      bench.Input(" 10,000 rows +k ", table_diff(10_000, False, True)),
      bench.Input(" 10,000 rows +ks", table_diff(10_000, True, True)),
      //
      bench.Input("100,000 rows    ", table_diff(100_000, False, False)),
      bench.Input("100,000 rows + s", table_diff(100_000, True, False)),
      bench.Input("100,000 rows +k ", table_diff(100_000, False, True)),
      bench.Input("100,000 rows +ks", table_diff(100_000, True, True)),
    ],
    [bench.Function("vdom.diff()", run_diff)],
    [bench.Duration(5000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> io.println()
}

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
