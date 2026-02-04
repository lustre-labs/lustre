// IMPORTS ---------------------------------------------------------------------

import details
import lustre
import lustre/attribute
import lustre/component
import lustre/element.{type Element}
import lustre/element/html
import lustre/platform/dom

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = dom.platform("#app")
  let app = lustre.simple(init, update, view)

  // Typically, it's important to register a component *before* your app starts.
  // This means the component's tag name – "my-counter" in this case - is registered
  // with the browser.
  let assert Ok(_) = details.register()
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

/// For this example, our main "app" doesn't have any state or functionality of
/// its own. Instead we'll see that each "my-counter" element contains its own
/// state and functionality.
///
type Model =
  Nil

fn init(_) -> Model {
  Nil
}

// UPDATE ----------------------------------------------------------------------

type Msg =
  Nil

fn update(_, _) -> Model {
  Nil
}

// VIEW ------------------------------------------------------------------------

fn view(_) -> Element(Msg) {
  html.div(
    [attribute.class("p-32 mx-auto w-full max-w-2xl flex flex-col gap-8")],
    [
      details.element([details.summary("Open me for a surprise!")], [
        html.img([
          attribute.class("aspect-square w-full max-w-2xl"),
          attribute.src("https://cdn2.thecatapi.com/images/8lg.gif"),
        ]),
      ]),
      details.element([], [
        html.p([component.slot("summary"), attribute.class("font-semibold")], [
          html.text("I have a surprise "),
          html.span([attribute.class("text-blue-500")], [html.text("too")]),
          html.text("!"),
        ]),
        html.img([
          attribute.class("aspect-square w-full max-w-2xl"),
          attribute.src("https://cdn2.thecatapi.com/images/8lg.gif"),
        ]),
      ]),
    ],
  )
}
