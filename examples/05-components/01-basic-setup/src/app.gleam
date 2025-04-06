// IMPORTS ---------------------------------------------------------------------

import counter
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)

  // Typically, it's important to register a component *before* your app starts.
  // This means the component's tag name – "my-counter" in this case - is registered
  // with the browser.
  let assert Ok(_) = counter.register()
  let assert Ok(_) = lustre.start(app, "#app", Nil)

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
  html.div([attribute.class("p-32 mx-auto w-full max-w-2xl")], [
    // Each "my-counter" element is a separate instance of the counter component,
    // with its own state and event handling.
    counter.element(),
    // If you inspect the DOM, you'll see these elements have the `<my-counter />`
    // tag!
    counter.element(),
    counter.element(),
  ])
}
