// IMPORTS ---------------------------------------------------------------------

import birdie
import lustre/attribute
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

pub fn void_elements_test() {
  use <- lustre_test.test_filter("void_elements_test")

  let input =
    html.div([], [
      html.area([]),
      html.base([]),
      html.br([]),
      html.col([]),
      html.embed([]),
      html.hr([]),
      html.img([]),
      html.input([]),
      html.link([]),
      html.meta([]),
      html.source([]),
      html.track([]),
      html.wbr([]),
    ])

  input |> snapshot("Void elements should render as such")
}

pub fn element_namespaced_void_test() {
  use <- lustre_test.test_filter("element_namespaced_void_test")

  let input =
    html.div([], [
      element.element("link", [], []),
      element.namespaced("", "link", [], []),
      element.namespaced("bitter-end", "link", [], []),
    ])

  input
  |> snapshot(
    "Semi-advanced tag functions should still have the HTML void checking",
  )
}

pub fn void_advanced_elements_test() {
  use <- lustre_test.test_filter("void_advanced_elements_test")

  let input =
    html.div([], [
      element.advanced("", "link", [], [element.text("Wibble")], False, False),
      element.advanced("", "link", [], [], False, True),
    ])

  input |> snapshot("Advanced elements should be able to be void and non-void")
}

pub fn keyed_void_elements_test() {
  use <- lustre_test.test_filter("keyed_void_elements_test")

  let input =
    keyed.div([], [
      #("area", html.area([])),
      #("base", html.base([])),
      #("br", html.br([])),
      #("col", html.col([])),
      #("embed", html.embed([])),
      #("hr", html.hr([])),
      #("img", html.img([])),
      #("input", html.input([])),
      #("link", html.link([])),
      #("meta", html.meta([])),
      #("source", html.source([])),
      #("track", html.track([])),
      #("wbr", html.wbr([])),
    ])

  input |> snapshot("Keyed void elements should render as such")
}

pub fn keyed_element_namespaced_void_test() {
  use <- lustre_test.test_filter("keyed_element_namespaced_void_test")

  let input =
    keyed.div([], [
      #("wee", element.element("link", [], [])),
      #("oo", element.namespaced("", "link", [], [])),
      #("goes the ambulance", element.namespaced("rapture", "link", [], [])),
    ])

  input
  |> snapshot(
    "Semi-advanced keyed tag functions should still have the HTML void checking",
  )
}

pub fn keyed_void_advanced_elements_test() {
  use <- lustre_test.test_filter("keyed_void_advanced_elements_test")

  let input =
    keyed.div([], [
      #(
        "non-void",
        element.advanced("", "link", [], [element.text("Wibble")], False, False),
      ),
      #("void", element.advanced("", "link", [], [], False, True)),
    ])

  input
  |> snapshot("Keyed advanced elements should be able to be void and non-void")
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

// ATTRIBUTE TESTS -------------------------------------------------------------

pub fn default_value_attribute_test() {
  use <- lustre_test.test_filter("default_value_attribute_test")

  let input =
    html.input([
      attribute.type_("text"),
      attribute.default_value("Hello, world!"),
    ])

  input
  |> snapshot("Default value attribute should be rendered as value attribute")
}

pub fn default_checked_attribute_test() {
  use <- lustre_test.test_filter("default_checked_attribute_test")

  let input =
    html.input([
      attribute.type_("checkbox"),
      attribute.default_checked(True),
    ])

  input
  |> snapshot(
    "Default checked attribute should be rendered as checked attribute",
  )
}

pub fn default_selected_attribute_test() {
  use <- lustre_test.test_filter("default_selected_attribute_test")

  let input =
    html.select([], [html.option([attribute.default_selected(True)], "")])

  input
  |> snapshot(
    "Default selected attribute should be rendered as selected attribute",
  )
}

// UTILS -----------------------------------------------------------------------

fn snapshot(el: Element(msg), title: String) -> Nil {
  el |> element.to_readable_string |> birdie.snap("[html] " <> title)
}
