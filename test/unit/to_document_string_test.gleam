// IMPORTS ---------------------------------------------------------------------

import gleam/string
import gleam/string_tree
import lustre/attribute
import lustre/element
import lustre/element/html
import lustre_test

// TO_DOCUMENT_STRING TESTS ----------------------------------------------------

pub fn to_document_string_html_element_test() {
  use <- lustre_test.test_filter("to_document_string_html_element_test")

  let page =
    html.html([], [
      html.head([], []),
      html.body([], [html.h1([], [html.text("Hello")])]),
    ])

  let result = element.to_document_string(page)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<html>")
  assert !double_html_body(result)
}

pub fn to_document_string_head_element_test() {
  use <- lustre_test.test_filter("to_document_string_head_element_test")

  let el = html.head([], [html.title([], "Test")])
  let result = element.to_document_string(el)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<html>")
  assert string.contains(result, "<head>")
}

pub fn to_document_string_body_element_test() {
  use <- lustre_test.test_filter("to_document_string_body_element_test")

  let el = html.body([], [html.p([], [html.text("Hi")])])
  let result = element.to_document_string(el)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<html>")
  assert string.contains(result, "<body>")
}

pub fn to_document_string_other_element_test() {
  use <- lustre_test.test_filter("to_document_string_other_element_test")

  let el = html.div([], [html.text("Hello")])
  let result = element.to_document_string(el)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<html><body>")
  assert string.contains(result, "<div>Hello</div>")
}

pub fn to_document_string_mapped_html_element_test() {
  use <- lustre_test.test_filter("to_document_string_mapped_html_element_test")

  let page =
    html.html([], [
      html.head([], [
        html.meta([attribute.attribute("charset", "utf-8")]),
        html.link([
          attribute.attribute("rel", "icon"),
          attribute.attribute("href", "/favicon.ico"),
        ]),
        html.title([], "Test Page"),
      ]),
      html.body([], [html.h1([], [html.text("Hello")])]),
    ])

  let mapped = element.map(page, fn(_) { Nil })
  let result = element.to_document_string(mapped)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<head>")
  assert string.contains(result, "<title>Test Page</title>")
  assert !double_html_body(result)
}

pub fn to_document_string_double_mapped_html_element_test() {
  use <- lustre_test.test_filter(
    "to_document_string_double_mapped_html_element_test",
  )

  let page =
    html.html([], [
      html.head([], []),
      html.body([], [html.text("Hi")]),
    ])

  let double_mapped =
    page
    |> element.map(fn(_) { Nil })
    |> element.map(fn(_) { Nil })

  let result = element.to_document_string(double_mapped)

  assert string.starts_with(result, "<!doctype html>\n")
  assert !double_html_body(result)
}

pub fn to_document_string_mapped_head_element_test() {
  use <- lustre_test.test_filter("to_document_string_mapped_head_element_test")

  let el = html.head([], [html.title([], "Test")])
  let mapped = element.map(el, fn(_) { Nil })
  let result = element.to_document_string(mapped)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<html>")
  assert string.contains(result, "<head>")
  assert !string.contains(result, "<body>")
}

pub fn to_document_string_mapped_body_element_test() {
  use <- lustre_test.test_filter("to_document_string_mapped_body_element_test")

  let el = html.body([], [html.p([], [html.text("Hi")])])
  let mapped = element.map(el, fn(_) { Nil })
  let result = element.to_document_string(mapped)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<html>")
  assert string.contains(result, "<body>")
}

pub fn to_document_string_mapped_other_element_test() {
  use <- lustre_test.test_filter("to_document_string_mapped_other_element_test")

  let el = html.div([], [html.text("Hello")])
  let mapped = element.map(el, fn(_) { Nil })
  let result = element.to_document_string(mapped)

  assert string.starts_with(result, "<!doctype html>\n")
  assert string.contains(result, "<html><body>")
}

// TO_DOCUMENT_STRING_TREE TESTS -----------------------------------------------

pub fn to_document_string_tree_mapped_html_element_test() {
  use <- lustre_test.test_filter(
    "to_document_string_tree_mapped_html_element_test",
  )

  let page =
    html.html([], [
      html.head([], [html.title([], "Test")]),
      html.body([], [html.text("Hi")]),
    ])

  let mapped = element.map(page, fn(_) { Nil })
  let result =
    element.to_document_string_tree(mapped)
    |> string_tree.to_string

  assert string.starts_with(result, "<!doctype html>\n")
  assert !double_html_body(result)
}

// UTILS -----------------------------------------------------------------------

/// Check if the output contains the telltale sign of the double-wrapping bug:
/// `<html><body>` appearing before the actual document content.
///
fn double_html_body(html: String) -> Bool {
  string.contains(html, "<html><body><!-- lustre:map -->")
}
