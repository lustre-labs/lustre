// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{attribute, class, disabled, src, style}
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
  div(
    [
      class("'badquotes'"),
      style([#("background", "\"><script>alert`1`</script>")]),
      attribute("example", "{\"mykey\": \"myvalue\"}"),
    ],
    [],
  )
}
