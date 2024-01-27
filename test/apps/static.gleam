// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{src}
import lustre/element.{text}
import lustre/element/html.{body, h1, head, html, img, title}

// VIEW ------------------------------------------------------------------------

pub fn view() {
  html([], [
    head([], [title([], "Hello, World!")]),
    body([], [
      h1([], [text("Hello, World!")]),
      img([src("https://source.unsplash.com/random")]),
    ]),
  ])
}
