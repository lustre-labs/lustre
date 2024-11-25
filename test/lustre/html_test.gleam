// IMPORTS ---------------------------------------------------------------------

import birdie
import lustre/element.{type Element}
import lustre/element/html

// ELEMENT TESTS ---------------------------------------------------------------

pub fn element_empty_test() {
  let input = html.div([], [])

  input |> snapshot("Empty element should have nothing between tags")
}

pub fn element_multiple_children_test() {
  let input = html.div([], [html.p([], []), html.p([], []), html.p([], [])])

  input |> snapshot("Element with multiple children should render in order")
}

// FRAGMENT TESTS --------------------------------------------------------------

pub fn fragment_empty_test() {
  let input = element.fragment([])

  input |> snapshot("Empty fragment should render nothing")
}

pub fn fragment_single_text_test() {
  let input = element.fragment([html.text("Hello, world!")])

  input |> snapshot("Fragment with single text node should render plain text")
}

pub fn fragment_single_element_test() {
  let input = element.fragment([html.div([], [])])

  input |> snapshot("Fragment with single element should render that element")
}

pub fn fragment_multiple_text_test() {
  let input = element.fragment([html.text("Hello, "), html.text("world!")])

  input
  |> snapshot(
    "Fragment with multiple text nodes should render concatenated text",
  )
}

pub fn fragment_multiple_elements_test() {
  let input = element.fragment([html.div([], []), html.div([], [])])

  input
  |> snapshot("Fragment with multiple elements should render on multiple lines")
}

pub fn fragment_multiple_mixed_test() {
  let input =
    element.fragment([
      html.text("Hello, "),
      html.span([], [html.text("world")]),
      html.text("!"),
    ])

  input
  |> snapshot(
    "Fragment with multiple mix elements should render on multiple lines",
  )
}

// UTILS -----------------------------------------------------------------------

fn snapshot(el: Element(msg), title: String) -> Nil {
  el |> element.to_readable_string |> birdie.snap("[html] " <> title)
}
