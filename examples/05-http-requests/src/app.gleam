import gleam/dynamic
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
// Lustre_http is a community package that provides a simple API for making
// HTTP requests from your update function. You can find the docs for the package
// here:
//
//   https://hexdocs.pm/lustre_http/index.html
import lustre_http.{type HttpError}
// These examples are written with `lustre/ui` in mind. They'll work regardless,
// but to see what `lustre/ui` can do make sure to run each of these examples with
// the `--use-lustre-ui` flag:
//
//   $ gleam run -m lustre dev --use-lustre-ui
//
// In your own apps, make sure to add the `lustre/ui` dependency and include the
// stylesheet somewhere.
import lustre/ui
import lustre/ui/aside

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(quote: Option(Quote))
}

type Quote {
  Quote(author: String, content: String)
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(Model(quote: None), effect.none())
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  GotQuote(Result(Quote, HttpError))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Refresh -> #(model, get_quote())
    GotQuote(Ok(quote)) -> #(Model(quote: Some(quote)), effect.none())
    GotQuote(Error(_)) -> #(model, effect.none())
  }
}

fn get_quote() -> Effect(Msg) {
  let url = "https://api.quotable.io/random"
  let decoder =
    dynamic.decode2(
      Quote,
      dynamic.field("author", dynamic.string),
      dynamic.field("content", dynamic.string),
    )

  lustre_http.get(url, lustre_http.expect_json(decoder, GotQuote))
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]

  ui.centre(
    [attribute.style(styles)],
    ui.aside(
      [aside.min_width(70), attribute.style([#("width", "60ch")])],
      view_quote(model.quote),
      ui.button([event.on_click(Refresh)], [element.text("New quote")]),
    ),
  )
}

fn view_quote(quote: Option(Quote)) -> Element(msg) {
  case quote {
    Some(quote) ->
      ui.stack([], [
        element.text(quote.author <> " once said..."),
        html.p([attribute.style([#("font-style", "italic")])], [
          element.text(quote.content),
        ]),
      ])
    None -> html.p([], [element.text("Click the button to get a quote!")])
  }
}
