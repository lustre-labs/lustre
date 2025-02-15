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
    html.div([attribute.class("container")], [
      html.div([attribute.class("jumbotron")], [
        html.div([attribute.class("row")], [
          html.div([attribute.class("col-md-6")], [
            html.h1([], [html.text("Vdom Benchmark")]),
          ]),
          html.div([attribute.class("col-md-6")], [
            html.div([attribute.class("row")], [
              html.div([attribute.class("col-sm-6 smallpad")], [
                html.button(
                  [
                    attribute.type_("button"),
                    attribute.class("btn btn-primary btn-block"),
                    attribute.id("run"),
                  ],
                  [html.text("Create 1,000 rows")],
                ),
              ]),
              html.div([attribute.class("col-sm-6 smallpad")], [
                html.button(
                  [
                    attribute.type_("button"),
                    attribute.class("btn btn-primary btn-block"),
                    attribute.id("runlots"),
                  ],
                  [html.text("Create 10,000 rows")],
                ),
              ]),
            ]),
          ]),
        ]),
      ]),
      element.keyed(
        html.table(
          [attribute.class("table table-hover table-striped test-data")],
          _,
        ),
        list.range(1, rows)
          |> list.map(fn(i) {
            #(
              int.to_string(i),
              html.tr([attribute.class("danger")], [
                html.td([attribute.class("col-md-1")], [
                  html.text(int.to_string(i)),
                ]),
                html.td([attribute.class("col-md-4")], [
                  html.a([], [html.text("Row " <> int.to_string(i))]),
                ]),
                html.td([attribute.class("col-md-1")], [
                  html.a([], [html.text("Delete")]),
                ]),
                html.td([attribute.class("col-md-4")], []),
              ]),
            )
          }),
      ),
    ])

  let next =
    html.div([attribute.class("container")], [
      html.div([attribute.class("jumbotron")], [
        html.div([attribute.class("row")], [
          html.div([attribute.class("col-md-6")], [
            html.h1([], [html.text("Vdom Benchmark")]),
          ]),
          html.div([attribute.class("col-md-6")], [
            html.div([attribute.class("row")], [
              html.div([attribute.class("col-sm-6 smallpad")], [
                html.button(
                  [
                    attribute.type_("button"),
                    attribute.class("btn btn-primary btn-block"),
                    attribute.id("run"),
                  ],
                  [html.text("Create 1,000 rows")],
                ),
              ]),
              html.div([attribute.class("col-sm-6 smallpad")], [
                html.button(
                  [
                    attribute.type_("button"),
                    attribute.class("btn btn-primary btn-block"),
                    attribute.id("runlots"),
                  ],
                  [html.text("Create 10,000 rows")],
                ),
              ]),
            ]),
          ]),
        ]),
      ]),
      element.keyed(
        html.table(
          [attribute.class("table table-hover table-striped test-data")],
          _,
        ),
        list.range(1 + { rows / 2 }, rows + { rows / 2 })
          |> list.shuffle()
          |> list.map(fn(i) {
            #(
              int.to_string(i),
              html.tr([attribute.class("danger")], [
                html.td([attribute.class("col-md-1")], [
                  html.text(int.to_string(i)),
                ]),
                html.td([attribute.class("col-md-4")], [
                  html.a([], [html.text("Updated Row " <> int.to_string(i))]),
                ]),
                html.td([attribute.class("col-md-1")], [
                  html.a([], [html.text("Delete")]),
                ]),
                html.td([attribute.class("col-md-4")], []),
              ]),
            )
          }),
      ),
    ])

  #(prev, next)
}
