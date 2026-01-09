// IMPORTS ---------------------------------------------------------------------

import argv
import benchmark/diff_benchmark
import gleeunit
import lustre/vdom/vnode.{type Element}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  case argv.load().arguments {
    ["benchmark", ..] -> {
      let _ = diff_benchmark.benchmark_10_rows()
      let _ = diff_benchmark.benchmark_100_rows()
      let _ = diff_benchmark.benchmark_1000_rows()
      let _ = diff_benchmark.benchmark_10_000_rows()

      Nil
    }

    _ -> gleeunit.main()
  }
}

/// Zed has a neat ui to run individual tests as a task that runs `gleam test`
/// with an argument `--test-name-filter=` followed by the name of the test
/// function.
///
/// Gleeunt doesn't have any test filtering built-in but we can fake it by
/// wrapping all of our tests in this function and passing in the name of the
/// test.
///
pub fn test_filter(name: String, f: fn() -> Nil) -> Nil {
  case argv.load().arguments {
    ["--test-name-filter=" <> test_name, ..] if test_name == name -> f()
    ["--test-name-filter=" <> _, ..] -> Nil

    // If the filter argument isn't provided we want to run every test!
    _ -> f()
  }
}

pub fn nodes_equal_ignoring_memo(left: Element(msg), right: Element(msg)) {
  do_nodes_equal(left, right, True)
}

pub fn nodes_equal(left: Element(msg), right: Element(msg)) {
  do_nodes_equal(left, right, False)
}

fn do_nodes_equal(left: Element(msg), right: Element(msg), ignore_memo: Bool) {
  case left, right {
    vnode.Fragment(..), vnode.Fragment(..) ->
      left.key == right.key
      && children_equal(left.children, right.children, ignore_memo)

    vnode.Map(..), vnode.Map(..) ->
      left.key == right.key
      && do_nodes_equal(left.child, right.child, ignore_memo)

    vnode.Memo(..), vnode.Memo(..) ->
      left.key == right.key
      && do_nodes_equal(left.view(), right.view(), ignore_memo)

    // // Memos are transparent in the DOM, so when virtualising we get what they evaluate to
    _, vnode.Memo(..) if ignore_memo ->
      do_nodes_equal(left, right.view(), ignore_memo)
    vnode.Memo(..), _ if ignore_memo ->
      do_nodes_equal(left.view(), right, ignore_memo)

    // don't check the key on text nodes - we can't virtualise it
    vnode.Text(..), vnode.Text(..) -> left.content == right.content

    vnode.Element(..), vnode.Element(..) ->
      left.key == right.key
      && left.tag == right.tag
      && left.namespace == right.namespace
      && left.attributes == right.attributes
      && children_equal(left.children, right.children, ignore_memo)

    vnode.UnsafeInnerHtml(..), vnode.UnsafeInnerHtml(..) ->
      left.key == right.key
      && left.tag == right.tag
      && left.namespace == right.namespace
      && left.attributes == right.attributes
      && left.inner_html == right.inner_html
    _, _ -> False
  }
}

fn children_equal(
  left: List(Element(msg)),
  right: List(Element(msg)),
  ignore_memo: Bool,
) -> Bool {
  case left, right {
    // base cases
    [], [] -> True
    [_, ..], [] | [], [_, ..] -> False

    // compare children
    [first_left, ..left], [first_right, ..right] ->
      case do_nodes_equal(first_left, first_right, ignore_memo) {
        True -> children_equal(left, right, ignore_memo)
        False -> False
      }
  }
}
