import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleamy/bench
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/runtime/vdom

pub fn run() {
  bench.run(
    [
      bench.Input("     10 rows", table_diff(10)),
      bench.Input("    100 rows", table_diff(100)),
      bench.Input("  1,000 rows", table_diff(1000)),
      bench.Input(" 10,000 rows", table_diff(10_000)),
      bench.Input("100,000 rows", table_diff(100_000)),
    ],
    [bench.Function("vdom.diff()", run_diff)],
    [bench.Duration(1000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> io.println()
}

fn run_diff(input: #(Element(msg), Element(msg))) {
  vdom.diff(input.0, input.1, dict.new())
}

fn table_diff(rows: Int) {
  let prev =
    element.keyed(
      html.table([], _),
      list.range(1, rows)
        |> list.map(fn(i) {
          #(
            int.to_string(i),
            html.tr([], [
              html.td([], [html.text(int.to_string(i))]),
              html.td([], [
                html.a([], [
                  html.text("Updated Row "),
                  html.text(int.to_string(i)),
                ]),
              ]),
              html.td([], [html.button([], [html.text("Delete")])]),
            ]),
          )
        }),
    )

  let next =
    element.keyed(
      html.table([], _),
      list.range(1 + { rows / 2 }, rows + { rows / 2 })
        |> list.map(fn(i) {
          #(
            int.to_string(i),
            html.tr([], [
              html.td([], [html.text(int.to_string(i))]),
              html.td([], [
                html.a([], [
                  html.text("Updated Row "),
                  html.text(int.to_string(i)),
                ]),
              ]),
              html.td([], [html.button([], [html.text("Delete")])]),
            ]),
          )
        }),
    )

  #(prev, next)
}
