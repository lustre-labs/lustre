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
fn test_virtualise(vdom: Element(msg)) {
  use virtualised <- virtualise(element.to_string(vdom))
  assert virtualised == vdom
}

// FFI -------------------------------------------------------------------------

@target(javascript)
@external(javascript, "./virtualise_test.ffi.mjs", "virtualise")
fn virtualise(html: String, callback: fn(Element(msg)) -> Nil) -> Nil
