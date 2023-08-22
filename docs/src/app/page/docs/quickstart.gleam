// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/string
import gleam/io
import lustre/element.{Element}

// PAGE ------------------------------------------------------------------------

pub fn view() -> Element(msg) {
  [title]
  |> io.debug
  |> string.join("\n")
  |> io.debug
  |> layout.docs
}

// CONTENT: TITLE --------------------------------------------------------------

const title: String = "# Quickstart
"
