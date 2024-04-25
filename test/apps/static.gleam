// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{attribute, disabled, src}
import lustre/element.{text}
import lustre/element/html.{body, div, h1, head, html, img, input, title}

// VIEW ------------------------------------------------------------------------

pub fn view() {
  html([], [
    head([], [title([], "Hello, World!")]),
    body([], [
      h1([], [text("Hello, World!")]),
      input([disabled(True)]),
      img([src("https://source.unsplash.com/random")]),
    ]),
  ])
}

pub fn escaped_attribute() {
  div([attribute("example", "{\"mykey\": \"myvalue\"}")], [])
}
