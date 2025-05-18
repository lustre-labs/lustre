// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/pair
import gleeunit/should
import lustre/attribute
import lustre/dev/query
import lustre/element
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/vdom/events
import lustre/vdom/path
import lustre_test

//

pub fn find_path_in_single_event_test() {
  use <- lustre_test.test_filter("find_path_in_single_event_test")

  let vdom =
    html.button(
      [event.on_click("hello!"), attribute.data("test-id", "target")],
      [html.text("Click me!")],
    )

  let events = events.from_node(vdom)
  let query = query.element(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok("hello!"))
}

pub fn find_path_in_single_nested_event_test() {
  use <- lustre_test.test_filter("find_path_in_single_nested_event_test")

  let vdom =
    html.div([], [
      html.button(
        [event.on_click("hello!"), attribute.data("test-id", "target")],
        [html.text("Click me!")],
      ),
    ])

  let events = events.from_node(vdom)
  let query = query.element(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok("hello!"))
}

// KEYS ------------------------------------------------------------------------

pub fn find_path_in_single_nested_keyed_event_test() {
  use <- lustre_test.test_filter("find_path_in_single_nested_keyed_event_test")

  let vdom =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #(
        "b",
        html.button(
          [event.on_click("hello!"), attribute.data("test-id", "target")],
          [html.text("Click me!")],
        ),
      ),
    ])

  let events = events.from_node(vdom)
  let query = query.element(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok("hello!"))
}

pub fn find_path_in_single_nested_keyed_event_with_period_test() {
  use <- lustre_test.test_filter(
    "find_path_in_single_nested_keyed_event_with_period_test",
  )

  let vdom =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #(
        "b.c",
        html.button(
          [event.on_click("hello!"), attribute.data("test-id", "target")],
          [html.text("Click me!")],
        ),
      ),
    ])

  let events = events.from_node(vdom)
  let query = query.element(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok("hello!"))
}

// FRAGMENTS -------------------------------------------------------------------

pub fn find_path_in_fragment_event_test() {
  use <- lustre_test.test_filter("find_path_in_fragment_event_test")

  let vdom =
    element.fragment([
      html.button(
        [event.on_click("hello!"), attribute.data("test-id", "target")],
        [html.text("Click me!")],
      ),
    ])

  let events = events.from_node(vdom)
  let query = query.element(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok("hello!"))
}

pub fn find_path_in_nested_fragment_event_test() {
  use <- lustre_test.test_filter("find_path_in_nested_fragment_event_test")

  let vdom =
    html.div([], [
      element.fragment([
        html.button(
          [event.on_click("hello!"), attribute.data("test-id", "target")],
          [html.text("Click me!")],
        ),
      ]),
    ])

  let events = events.from_node(vdom)
  let query = query.element(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok("hello!"))
}

pub fn find_path_in_nested_fragment_with_multiple_children_event_test() {
  use <- lustre_test.test_filter(
    "find_path_in_nested_fragment_with_multiple_children_event_test",
  )

  let vdom =
    html.div([], [
      element.fragment([
        html.button([event.on_click(1)], [html.text("Button 1!")]),
        element.fragment([
          html.button([event.on_click(2)], [html.text("Button 2!")]),
          html.button([event.on_click(3)], [html.text("Button 3!")]),
        ]),
        html.button([event.on_click(4), attribute.data("test-id", "target")], [
          html.text("Button 4!"),
        ]),
      ]),
    ])

  let events = events.from_node(vdom)
  let query = query.element(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok(4))
}

// CHILD QUERIES ---------------------------------------------------------------

pub fn find_path_by_child_query_test() {
  use <- lustre_test.test_filter("find_path_by_child_query_test")

  let vdom =
    html.div([], [
      html.button([event.on_click(1)], [html.text("Button 1!")]),
      html.button([event.on_click(2), attribute.data("test-id", "target")], [
        html.text("Button 2!"),
      ]),
    ])

  let events = events.from_node(vdom)
  let query =
    query.element(matching: query.tag("div"))
    |> query.child(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok(2))
}

pub fn find_path_by_child_query_in_fragment_test() {
  use <- lustre_test.test_filter("find_path_by_child_query_in_fragment_test")

  let vdom =
    html.div([], [
      html.button([event.on_click(1)], [html.text("Button 1!")]),
      element.fragment([
        html.button([event.on_click(2), attribute.data("test-id", "target")], [
          html.text("Button 2!"),
        ]),
      ]),
    ])

  let events = events.from_node(vdom)
  let query =
    query.element(matching: query.tag("div"))
    |> query.child(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok(2))
}

pub fn find_path_by_descendant_query_test() {
  use <- lustre_test.test_filter("find_path_by_descendant_query_test")

  let vdom =
    html.div([], [
      html.div([], [
        html.button([event.on_click(1)], [html.text("Button 1!")]),
        html.button([event.on_click(2), attribute.data("test-id", "target")], [
          html.text("Button 2!"),
        ]),
      ]),
    ])

  let events = events.from_node(vdom)
  let query =
    query.element(matching: query.tag("div"))
    |> query.descendant(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok(2))
}

pub fn find_path_by_descendant_query_in_fragment_test() {
  use <- lustre_test.test_filter(
    "find_path_by_descendant_query_in_fragment_test",
  )

  let vdom =
    html.div([], [
      html.div([], [
        html.button([event.on_click(1)], [html.text("Button 1!")]),
        element.fragment([
          html.button([event.on_click(2), attribute.data("test-id", "target")], [
            html.text("Button 2!"),
          ]),
        ]),
      ]),
    ])

  let events = events.from_node(vdom)
  let query =
    query.element(matching: query.tag("div"))
    |> query.descendant(matching: query.test_id("target"))
  let assert Ok(#(_, path)) = query.find_path(vdom, query, 0, path.root)

  events.handle(events, path.to_string(path), "click", dynamic.nil())
  |> pair.second
  |> should.equal(Ok(2))
}
