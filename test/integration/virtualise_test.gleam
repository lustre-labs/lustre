@target(javascript)
import lustre/element.{type Element}
@target(javascript)
import lustre/element/html
@target(javascript)
import lustre/element/keyed
@target(javascript)
import lustre_test

@target(javascript)
pub fn virtualise_none_test() {
  use <- lustre_test.test_filter("virtualise_none_test")
  test_virtualise(element.none())
}

@target(javascript)
pub fn virtualise_empty_div_test() {
  use <- lustre_test.test_filter("virtualise_empty_div_test")
  test_virtualise(html.div([], []))
}

@target(javascript)
pub fn virtualise_fragment_root_test() {
  use <- lustre_test.test_filter("virtualise_fragment_root_test")
  test_virtualise(element.fragment([html.div([], []), html.div([], [])]))
}

@target(javascript)
pub fn virtualise_text_test() {
  use <- lustre_test.test_filter("virtualise_text_test")
  test_virtualise(html.text("Hello, Joe!"))
}

@target(javascript)
pub fn virtualise_tree_test() {
  use <- lustre_test.test_filter("virtualise_tree_test")
  let html =
    html.div([], [
      html.h1([], [html.text("Welcome!")]),
      html.p([], [html.text("...")]),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_keyed_test() {
  use <- lustre_test.test_filter("virtualise_keyed_test")

  let html =
    keyed.div([], [
      #("a", html.h1([], [html.text("Welcome!")])),
      #("b", html.p([], [html.text("...")])),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_map_test() {
  use <- lustre_test.test_filter("virtualise_map_test")

  let html =
    html.div([], [
      element.map(html.h1([], [html.text("Hello")]), fn(x) { x }),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_map_with_fragment_test() {
  use <- lustre_test.test_filter("virtualise_map_with_fragment_test")

  let html =
    html.div([], [
      element.map(
        element.fragment([html.h1([], [html.text("A")]), html.p([], [])]),
        fn(x) { x },
      ),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_nested_map_test() {
  use <- lustre_test.test_filter("virtualise_nested_map_test")

  let html =
    html.div([], [
      element.map(
        element.map(html.h1([], [html.text("Nested")]), fn(x) { x }),
        fn(x) { x },
      ),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_memo_test() {
  use <- lustre_test.test_filter("virtualise_memo_test")

  let html =
    html.div([], [element.memo([], fn() { html.h1([], [html.text("Memo")]) })])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_memo_with_fragment_test() {
  use <- lustre_test.test_filter("virtualise_memo_with_fragment_test")

  let html =
    html.div([], [
      element.memo([], fn() {
        element.fragment([html.h1([], [html.text("A")]), html.p([], [])])
      }),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_memo_with_map_test() {
  use <- lustre_test.test_filter("virtualise_memo_with_map_test")

  let html =
    html.div([], [
      element.memo([], fn() {
        element.map(html.h1([], [html.text("MapInMemo")]), fn(x) { x })
      }),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_map_with_memo_test() {
  use <- lustre_test.test_filter("virtualise_map_with_memo_test")

  let html =
    html.div([], [
      element.map(
        element.memo([], fn() { html.h1([], [html.text("MemoInMap")]) }),
        fn(x) { x },
      ),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_fragment_with_map_and_memo_test() {
  use <- lustre_test.test_filter("virtualise_fragment_with_map_and_memo_test")

  let html =
    element.fragment([
      element.map(html.h1([], [html.text("Mapped")]), fn(x) { x }),
      element.memo([], fn() { html.p([], [html.text("Memoized")]) }),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_complex_nested_test() {
  use <- lustre_test.test_filter("virtualise_complex_nested_test")

  let html =
    html.div([], [
      element.memo([], fn() {
        element.fragment([
          element.map(html.h1([], [html.text("Title")]), fn(x) { x }),
          element.memo([], fn() {
            element.fragment([
              html.p([], [html.text("Content")]),
              element.map(html.span([], [html.text("Span")]), fn(x) { x }),
            ])
          }),
        ])
      }),
    ])

  test_virtualise(html)
}

@target(javascript)
pub fn virtualise_external_html_fragment_test() {
  use <- lustre_test.test_filter("virtualise_external_html_fragment_test")

  let html = "<p>Hello</p><p>World</p>"
  let vdom =
    element.fragment([
      html.p([], [html.text("Hello")]),
      html.p([], [html.text("World")]),
    ])

  test_external_virtualise(html, vdom)
}

@target(javascript)
pub fn virtualise_external_html_fragment_with_comments_test() {
  use <- lustre_test.test_filter(
    "virtualise_external_html_fragment_with_comments_test",
  )

  let html = "<!-- ignore me --><p>Hello</p><!-- and me --><p>World</p>"
  let vdom =
    element.fragment([
      html.p([], [html.text("Hello")]),
      html.p([], [html.text("World")]),
    ])

  test_external_virtualise(html, vdom)
}

@target(javascript)
pub fn virtualise_external_single_element_with_comments_test() {
  use <- lustre_test.test_filter(
    "virtualise_external_single_element_with_comments_test",
  )

  let html = "<!-- ignore me --><p>Hello</p><!-- and me -->"
  let vdom = html.p([], [html.text("Hello")])

  test_external_virtualise(html, vdom)
}

// UTILS -----------------------------------------------------------------------

@target(javascript)
fn test_virtualise(vdom: Element(message)) {
  use virtualised <- virtualise(element.to_string(vdom))
  assert lustre_test.nodes_equal(virtualised, vdom)
}

@target(javascript)
fn test_external_virtualise(html: String, vdom: Element(message)) {
  use virtualised <- virtualise(html)
  assert lustre_test.nodes_equal(virtualised, vdom)
}

// FFI -------------------------------------------------------------------------

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "virtualise")
fn virtualise(html: String, callback: fn(Element(message)) -> Nil) -> Nil
