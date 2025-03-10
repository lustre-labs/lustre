// IMPORTS ---------------------------------------------------------------------

import birdie
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre_test

// ELEMENT TESTS ---------------------------------------------------------------

pub fn element_empty_test() {
  use <- lustre_test.test_filter("element_empty_test")

  let input = html.div([], [])

  input |> snapshot("Empty element should have nothing between tags")
}

pub fn element_multiple_children_test() {
  use <- lustre_test.test_filter("element_multiple_children_test")

  let input = html.div([], [html.p([], []), html.p([], []), html.p([], [])])

  input |> snapshot("Element with multiple children should render in order")
}

// FRAGMENT TESTS --------------------------------------------------------------

pub fn fragment_empty_test() {
  use <- lustre_test.test_filter("fragment_empty_test")

  let input = element.fragment([])

  input |> snapshot("Empty fragment should render nothing")
}

pub fn fragment_single_text_test() {
  use <- lustre_test.test_filter("fragment_single_text_test")

  let input = element.fragment([html.text("Hello, world!")])

  input |> snapshot("Fragment with single text node should render plain text")
}

pub fn fragment_single_element_test() {
  use <- lustre_test.test_filter("fragment_single_element_test")

  let input = element.fragment([html.div([], [])])

  input |> snapshot("Fragment with single element should render that element")
}

pub fn fragment_multiple_text_test() {
  use <- lustre_test.test_filter("fragment_multiple_text_test")

  let input = element.fragment([html.text("Hello, "), html.text("world!")])

  input
  |> snapshot(
    "Fragment with multiple text nodes should render concatenated text",
  )
}

pub fn fragment_multiple_elements_test() {
  use <- lustre_test.test_filter("fragment_multiple_elements_test")

  let input = element.fragment([html.div([], []), html.div([], [])])

  input
  |> snapshot("Fragment with multiple elements should render on multiple lines")
}

pub fn fragment_multiple_mixed_test() {
  use <- lustre_test.test_filter("fragment_multiple_mixed_test")

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

pub fn keyed_fragment_test() {
  use <- lustre_test.test_filter("keyed_fragment_test")

  let input =
    keyed.fragment([
      #("a", html.div([], [html.text("a")])),
      #("b", html.div([], [html.text("b")])),
      #("c", html.div([], [html.text("c")])),
    ])

  input
  |> snapshot("Keyed fragment adds keys to its children")
}

pub fn fragment_with_key_prefixes_children_test() {
  use <- lustre_test.test_filter("fragment_with_key_prefixes_children_test")

  let input =
    keyed.fragment([
      #(
        "a",
        keyed.fragment([
          #("a", html.div([], [html.text("a")])),
          #("b", html.div([], [html.text("b")])),
          #(
            "c",
            keyed.div([], [
              #("not-prefixed", html.div([], [html.text("not-prefixed")])),
            ]),
          ),
        ]),
      ),
    ])

  input
  |> snapshot("Fragment with key prefixes all children")
}

pub fn deep_keyed_fragment_children_prefixes_test() {
  use <- lustre_test.test_filter("deep_keyed_children_prefixes_test")

  let input =
    keyed.fragment([
      #(
        "a",
        element.fragment([
          keyed.fragment([
            #("a", html.div([], [html.text("a")])),
            #("b", html.div([], [html.text("b")])),
          ]),
          element.fragment([
            keyed.fragment([
              #(
                "a",
                keyed.div([], [
                  #("not-prefixed", html.div([], [html.text("not-prefixed")])),
                ]),
              ),
            ]),
          ]),
        ]),
      ),
    ])

  input
  |> snapshot(
    "Keyed children are still prefixed if there is a non-prefixed fragment in-between",
  )
}

// UTILS -----------------------------------------------------------------------

fn snapshot(el: Element(msg), title: String) -> Nil {
  el |> element.to_readable_string |> birdie.snap("[html] " <> title)
}
