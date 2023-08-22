// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/string
import lustre/element.{Element}

// PAGE ------------------------------------------------------------------------

pub fn view() -> Element(msg) {
  [title]
  |> string.join("\n")
  |> layout.docs
}

// CONTENT: TITLE --------------------------------------------------------------

const title: String = "
# Components
"
