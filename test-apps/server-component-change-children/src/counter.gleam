import gleam/list
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/ui

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.simple(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Model =
  List(#(String, String, String, String))

fn init(_) -> Model {
  []
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
}

fn update(_: Model, msg: Msg) -> Model {
  case msg {
    Incr -> [
      #("1", "1", "1", "1"),
      #("2", "2", "2", "2"),
      #("3", "3", "3", "3"),
      #("4", "4", "4", "4"),
      #("5", "5", "5", "5"),
    ]

    Decr -> [
      #("3", "3", "3", "3"),
      #("2", "2", "2", "2"),
      #("1", "1", "1", "1"),
    ]
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]

  ui.centre(
    [attribute.style(styles)],
    ui.stack([], [
      ui.button([event.on_click(Incr)], [element.text("ascending")]),
      ui.button([event.on_click(Decr)], [element.text("descending")]),
      html.div([], [
        ui.stack([], [
          html.table([], [
            html.thead([], [
              html.tr([attribute.style([])], [
                html.th([], [html.text("Part No")]),
                html.th([], [html.text("Customer")]),
                html.th([], [html.text("Job No")]),
                html.th([], [html.text("Due Date")]),
              ]),
            ]),
            {
              // let rows =
              html.tbody([], {
                list.map(model, fn(tuple) {
                  html.tr([], [
                    html.td([], [html.text(tuple.0)]),
                    html.td([], [html.text(tuple.1)]),
                    html.td([], [html.text(tuple.2)]),
                    html.td([], [html.text(tuple.3)]),
                  ])
                })
              })
            },
          ]),
        ]),
      ]),
    ]),
  )
}
