// IMPORTS ---------------------------------------------------------------------

import gleam/list
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/platform/dom

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = dom.platform("#app")
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  List(String)

fn init(_) -> Model {
  ["8le", "bm2", "8pg", "9ev", "9pm", "oc"]
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserClickedCycle
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserClickedCycle ->
      case model {
        [head, ..rest] -> list.append(rest, [head])
        [] -> []
      }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  // Take just the first three images to display
  let images = list.take(model, 3)

  html.div([attribute.class("w-full max-w-2xl mx-auto flex flex-col gap-4")], [
    view_unkeyed_list(images),
    view_keyed_list(images),
    html.button(
      [
        event.on_click(UserClickedCycle),
        attribute.class("w-full bg-orange-500 text-white py-1 rounded"),
      ],
      [html.text("Next")],
    ),
  ])
}

fn view_unkeyed_list(images: List(String)) -> Element(msg) {
  html.div([], [
    html.h2([attribute.class("text-2xl py-2")], [html.text("Unkeyed")]),
    // This app includes a small script to flash elements that have been updated
    // in the DOM. Notice how *every* image flashes in the unkeyed list. That's
    // because to Lustre it looks like the `"src"` attribute for each image changes
    // every time.
    html.ul([attribute.class("grid grid-cols-3 gap-2")], {
      list.map(images, view_cat)
    }),
  ])
}

fn view_keyed_list(images: List(String)) -> Element(msg) {
  html.div([], [
    html.h2([attribute.class("text-2xl py-2")], [html.text("Keyed")]),
    // Only the third image flashes in the keyed list. By keying the elements
    // Lustre is able to recognise that the first two images are the same as last
    // render, and so they can be reused!
    keyed.ul([attribute.class("grid grid-cols-3 gap-2")], {
      list.map(images, fn(id) { #(id, view_cat(id)) })
    }),
  ])
}

fn view_cat(id: String) -> Element(msg) {
  html.img([
    attribute.class("aspect-square rounded"),
    attribute.src("https://cdn2.thecatapi.com/images/" <> id <> ".jpg"),
  ])
}
