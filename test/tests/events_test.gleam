// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/function
import gleam/string
import gleeunit/should
import lustre/element
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/vdom/diff
import lustre/vdom/events
import lustre_test

//

pub fn single_event_test() {
  use <- lustre_test.test_filter("single_event_test")

  let vdom = html.button([event.on_click("hello!")], [html.text("Click me!")])

  let events =
    events.new(function.identity)
    |> events.add_child(function.identity, 0, vdom)

  let path = [dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}

pub fn single_nested_event_test() {
  use <- lustre_test.test_filter("single_nested_event_test")

  let vdom =
    html.div([], [
      html.button([event.on_click("hello!")], [html.text("Click me!")]),
    ])

  let events =
    events.new(function.identity)
    |> events.add_child(function.identity, 0, vdom)

  let path = [dynamic.from(0), dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}

pub fn single_nested_keyed_event_test() {
  use <- lustre_test.test_filter("single_nested_keyed_event_test")

  let vdom =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])

  let events =
    events.new(function.identity)
    |> events.add_child(function.identity, 0, vdom)

  let path = [dynamic.from(0), dynamic.from("b")]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}

//

pub fn fragment_event_test() {
  use <- lustre_test.test_filter("fragment_event_test")

  let vdom =
    element.fragment([
      html.button([event.on_click("hello!")], [html.text("Click me!")]),
    ])

  let events =
    events.new(function.identity)
    |> events.add_child(function.identity, 0, vdom)

  let path = [dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}

pub fn nested_fragment_event_test() {
  use <- lustre_test.test_filter("nested_fragment_event_test")

  let vdom =
    html.div([], [
      element.fragment([
        html.button([event.on_click("hello!")], [html.text("Click me!")]),
      ]),
    ])

  let events =
    events.new(function.identity)
    |> events.add_child(function.identity, 0, vdom)

  let path = [dynamic.from(0), dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}

//

pub fn single_mapped_event_test() {
  use <- lustre_test.test_filter("single_mapped_event_test")

  let vdom =
    element.map(
      html.button([event.on_click("hello!")], [html.text("Click me!")]),
      string.uppercase,
    )

  let events =
    events.new(function.identity)
    |> events.add_child(function.identity, 0, vdom)

  let path = [dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("HELLO!"))
}

pub fn multiple_mapped_event_test() {
  use <- lustre_test.test_filter("multiple_mapped_event_test")

  let vdom =
    element.map(
      element.map(
        html.button([event.on_click("hello!")], [html.text("Click me!")]),
        string.uppercase,
      ),
      string.append(_, "!"),
    )

  let events =
    events.new(function.identity)
    |> events.add_child(function.identity, 0, vdom)

  let path = [dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("HELLO!!"))
}

// DIFF TESTS ------------------------------------------------------------------

pub fn event_added_test() {
  use <- lustre_test.test_filter("event_added_test")

  let prev = html.button([], [html.text("Click me!")])
  let next = html.button([event.on_click("hello!")], [html.text("Click me!")])

  let events = diff.diff(prev, next, 0).events

  let path = [dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}

pub fn event_removed_test() {
  use <- lustre_test.test_filter("event_removed_test")

  let prev = html.button([event.on_click("hello!")], [html.text("Click me!")])
  let next = html.button([], [html.text("Click me!")])

  let events = diff.diff(prev, next, 0).events

  let path = [dynamic.from(0)]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Error([]))
}

pub fn element_added_test() {
  use <- lustre_test.test_filter("element_added_test")

  let prev = keyed.div([], [#("a", html.h1([], [html.text("Testing...")]))])
  let next =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])

  let events = diff.diff(prev, next, 0).events

  let path = [dynamic.from(0), dynamic.from("b")]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}

pub fn element_removed_test() {
  use <- lustre_test.test_filter("element_removed_test")

  let prev =
    keyed.div([], [
      #("a", html.h1([], [html.text("Testing...")])),
      #("b", html.button([event.on_click("hello!")], [html.text("Click me!")])),
    ])
  let next = keyed.div([], [#("a", html.h1([], [html.text("Testing...")]))])

  let events = diff.diff(prev, next, 0).events

  let path = [dynamic.from(0), dynamic.from("b")]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Error([]))
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

  let events = diff.diff(prev, next, 0).events

  let path = [dynamic.from(0), dynamic.from("b")]

  events.handle(events, path, "click", dynamic.from(Nil))
  |> should.equal(Ok("hello!"))
}
