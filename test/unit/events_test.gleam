// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/list
import gleam/string
import lustre/element
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/vdom/diff
import lustre/vdom/events
import lustre/vdom/path
import lustre/vdom/vattr.{Handler}
import lustre_test

//

pub fn single_event_test() {
  use <- lustre_test.test_filter("single_event_test")

  let vdom = html.button([event.on_click("hello!")], [html.text("Click me!")])

  let events = events.from_node(vdom)

  let path = "0"

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) = events.handle(events, path, "click", dynamic.nil())

  assert expected == actual
}

pub fn single_nested_event_test() {
  use <- lustre_test.test_filter("single_nested_event_test")

  let vdom =
    html.div([], [
      html.button([event.on_click("hello!")], [html.text("Click me!")]),
    ])

  let events = events.from_node(vdom)

  let path = path.root |> path.add(0, "") |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn single_nested_keyed_event_test() {
  use <- lustre_test.test_filter("single_nested_keyed_event_test")

  let vdom =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])

  let events = events.from_node(vdom)

  let path = path.root |> path.add(0, "") |> path.add(1, "b")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn single_nested_keyed_event_with_period_test() {
  use <- lustre_test.test_filter("single_nested_keyed_event_with_period_test")

  let vdom =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #(
        "b.c",
        html.button([event.on_click("hello!")], [html.text("Click me!")]),
      ),
    ])

  let events = events.from_node(vdom)

  let path = path.root |> path.add(0, "") |> path.add(1, "b.c")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

//

pub fn fragment_event_test() {
  use <- lustre_test.test_filter("fragment_event_test")

  let vdom =
    element.fragment([
      html.button([event.on_click("hello!")], [html.text("Click me!")]),
    ])

  let events = events.from_node(vdom)

  let path = "1"

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) = events.handle(events, path, "click", dynamic.nil())

  assert expected == actual
}

pub fn nested_fragment_event_test() {
  use <- lustre_test.test_filter("nested_fragment_event_test")

  let vdom =
    html.div([], [
      element.fragment([
        html.button([event.on_click("hello!")], [html.text("Click me!")]),
      ]),
    ])

  let events = events.from_node(vdom)

  let path = path.root |> path.add(0, "") |> path.add(1, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn nested_fragment_with_multiple_children_event_test() {
  use <- lustre_test.test_filter(
    "nested_fragment_with_multiple_children_event_test",
  )

  let vdom =
    html.div([], [
      element.fragment([
        html.button([event.on_click(1)], [html.text("Button 1!")]),
        element.fragment([
          html.button([event.on_click(2)], [html.text("Button 2!")]),
          html.button([event.on_click(3)], [html.text("Button 3!")]),
        ]),
        html.button([event.on_click(4)], [html.text("Button 4!")]),
      ]),
    ])

  let events = events.from_node(vdom)

  let path = path.root |> path.add(0, "") |> path.add(5, "")

  let expected =
    Ok(Handler(prevent_default: False, stop_propagation: False, message: 4))
  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

//

pub fn single_mapped_event_test() {
  use <- lustre_test.test_filter("single_mapped_event_test")

  let vdom =
    element.map(
      html.button([event.on_click("hello!")], [html.text("Click me!")]),
      string.uppercase,
    )

  let events = events.from_node(vdom)

  let path = path.root |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "HELLO!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn multiple_mapped_event_test() {
  use <- lustre_test.test_filter("multiple_mapped_event_test")

  let vdom =
    element.map(
      element.map(
        html.button([event.on_click("hello!")], [html.text("Click me!")]),
        string.uppercase,
      ),
      list.repeat(_, 2),
    )

  let events = events.from_node(vdom)

  let path = path.root |> path.add(0, "")

  let expected =
    Ok(
      Handler(prevent_default: False, stop_propagation: False, message: [
        "HELLO!", "HELLO!",
      ]),
    )

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

// DIFF TESTS ------------------------------------------------------------------

pub fn event_added_test() {
  use <- lustre_test.test_filter("event_added_test")

  let prev = html.button([], [html.text("Click me!")])
  let next = html.button([event.on_click("hello!")], [html.text("Click me!")])

  let events = diff.diff(events.new(), prev, next).events

  let path = path.root |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn event_removed_test() {
  use <- lustre_test.test_filter("event_removed_test")

  let prev = html.button([event.on_click("hello!")], [html.text("Click me!")])
  let next = html.button([], [html.text("Click me!")])

  let events = diff.diff(events.new(), prev, next).events

  let path = path.root |> path.add(0, "")

  let expected = Error([])
  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn element_added_test() {
  use <- lustre_test.test_filter("element_added_test")

  let prev = keyed.div([], [#("a", html.h1([], [html.text("Testing...")]))])
  let next =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])

  let events = diff.diff(events.new(), prev, next).events

  let path = path.root |> path.add(0, "") |> path.add(1, "b")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn element_removed_test() {
  use <- lustre_test.test_filter("element_removed_test")

  let prev =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])
  let next = keyed.div([], [#("a", html.h1([], [html.text("Testing...")]))])

  let events = diff.diff(events.new(), prev, next).events

  let path = path.root |> path.add(0, "") |> path.add(1, "b")

  let expected = Error([])
  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn element_replaced_test() {
  use <- lustre_test.test_filter("element_replaced_test")

  let prev =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])

  let next =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])

  let events = diff.diff(events.new(), prev, next).events

  let path = path.root |> path.add(0, "") |> path.add(1, "b")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello!",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}

pub fn keyed_element_replaced_test() {
  use <- lustre_test.test_filter("keyed_element_replaced_test")

  let prev =
    keyed.div([], [
      #(
        "v1",
        html.div([], [
          html.button([event.on_click("hello from 1")], [html.text("button 1")]),
          html.div([], [
            html.button([event.on_click("hello from 2")], [
              html.text("button 2"),
            ]),
          ]),
        ]),
      ),
    ])

  let next =
    keyed.div([], [
      #(
        "v2",
        html.div([], [
          html.button([event.on_click("hello from 1")], [html.text("button 1")]),
          html.div([], [
            html.button([event.on_click("hello from 2")], [
              html.text("button 2"),
            ]),
          ]),
        ]),
      ),
    ])

  let events = diff.diff(events.new(), prev, next).events

  let path =
    path.root |> path.add(0, "") |> path.add(0, "v2") |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello from 1",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual

  let path =
    path.root
    |> path.add(0, "")
    |> path.add(0, "v2")
    |> path.add(1, "")
    |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "hello from 2",
    ))

  let #(_, actual) =
    events.handle(events, path.to_string(path), "click", dynamic.nil())

  assert expected == actual
}
