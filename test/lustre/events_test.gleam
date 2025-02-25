import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleeunit/should
import lustre/element
import lustre/element/html
import lustre/internals/events
import lustre/runtime/vdom
import lustre_test

// INITIAL ID ASSIGNMENTS ------------------------------------------------------

pub fn no_events_init_test() {
  use <- lustre_test.test_filter("no_events_init_test")

  let prev = html.div([], [html.h1([], [html.text("Hello, Joe!")])])
  let events = vdom.init(prev)

  events |> should.equal(events.new())
}

pub fn single_event_init_test() {
  use <- lustre_test.test_filter("single_event_init_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let events = vdom.init(prev)

  events.handlers |> dict.get(0) |> should.equal(Ok(click.handler))
  events.ids |> dict.get(click.handler) |> should.equal(Ok(0))
}

pub fn multi_event_init_test() {
  use <- lustre_test.test_filter("multi_event_init_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let keydown =
    vdom.Event(
      name: "keydown",
      handler: decode.success(2),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
      html.input([keydown]),
    ])

  let events = vdom.init(prev)

  events.handlers |> dict.get(0) |> should.equal(Ok(click.handler))
  events.ids |> dict.get(click.handler) |> should.equal(Ok(0))

  events.handlers |> dict.get(1) |> should.equal(Ok(keydown.handler))
  events.ids |> dict.get(keydown.handler) |> should.equal(Ok(1))
}

// EVENT DIFFS -----------------------------------------------------------------

pub fn diff_no_event_changes_test() {
  use <- lustre_test.test_filter("diff_no_event_changes_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let prev_events = vdom.init(prev)

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let diff = vdom.diff(0, prev, next, prev_events)

  diff.events |> should.equal(prev_events)
}

pub fn diff_added_event_test() {
  use <- lustre_test.test_filter("diff_added_event_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev = html.div([], [html.h1([], [html.text("Hello, Joe!")])])
  let prev_events = vdom.init(prev)

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let diff = vdom.diff(0, prev, next, prev_events)

  diff.events.handlers |> dict.get(0) |> should.equal(Ok(click.handler))
  diff.events.ids |> dict.get(click.handler) |> should.equal(Ok(0))
}

pub fn diff_removed_event_test() {
  use <- lustre_test.test_filter("diff_removed_event_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let keydown =
    vdom.Event(
      name: "keydown",
      handler: decode.success(2),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
      html.input([keydown]),
    ])

  let prev_events = vdom.init(prev)

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let diff = vdom.diff(0, prev, next, prev_events)

  diff.events.handlers |> dict.get(0) |> should.equal(Ok(click.handler))
  diff.events.ids |> dict.get(click.handler) |> should.equal(Ok(0))

  diff.events.handlers |> dict.get(1) |> should.equal(Error(Nil))
  diff.events.ids |> dict.get(keydown.handler) |> should.equal(Error(Nil))
}

pub fn diff_added_removed_event_test() {
  use <- lustre_test.test_filter("diff_removed_event_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let keydown =
    vdom.Event(
      name: "keydown",
      handler: decode.success(2),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let prev_events = vdom.init(prev)

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.input([keydown]),
    ])

  let diff = vdom.diff(0, prev, next, prev_events)

  diff.events.handlers |> dict.get(0) |> should.equal(Error(Nil))
  diff.events.ids |> dict.get(click.handler) |> should.equal(Error(Nil))

  diff.events.handlers |> dict.get(1) |> should.equal(Ok(keydown.handler))
  diff.events.ids |> dict.get(keydown.handler) |> should.equal(Ok(1))
}

// MAPPING HANDLERS ------------------------------------------------------------

pub fn event_mapped_once_test() {
  use <- lustre_test.test_filter("event_mapped_once_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), int.add(_, 2)),
    ])

  let events = vdom.init(prev)
  let handler = events.handlers |> dict.get(0) |> should.be_ok

  decode.run(dynamic.from(Nil), handler) |> should.equal(Ok(3))
}

pub fn event_mapped_twice_test() {
  use <- lustre_test.test_filter("event_mapped_twice_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    element.map(
      html.div([], [
        html.h1([], [html.text("Hello, Joe!")]),
        element.map(html.button([click], [html.text("Click me!")]), int.add(
          _,
          2,
        )),
      ]),
      int.multiply(_, 2),
    )

  let events = vdom.init(prev)
  let handler = events.handlers |> dict.get(0) |> should.be_ok

  decode.run(dynamic.from(Nil), handler) |> should.equal(Ok(6))
}

pub fn mapped_fragment_test() {
  use <- lustre_test.test_filter("mapped_fragment_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    element.map(
      element.fragment([html.button([click], [html.text("Click me!")])]),
      int.add(_, 2),
    )

  let events = vdom.init(prev)
  let handler = events.handlers |> dict.get(0) |> should.be_ok

  decode.run(dynamic.from(Nil), handler) |> should.equal(Ok(3))
}

pub fn event_mapper_removed_test() {
  use <- lustre_test.test_filter("event_mapper_removed_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), int.add(_, 2)),
    ])

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let events = vdom.init(prev)
  let diff = vdom.diff(0, prev, next, events)
  let handler = diff.events.handlers |> dict.get(0) |> should.be_ok

  decode.run(dynamic.from(Nil), handler) |> should.equal(Ok(1))
}

pub fn event_mapper_changed_test() {
  use <- lustre_test.test_filter("event_mapper_changed_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), int.add(_, 2)),
    ])

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), int.multiply(
        _,
        10,
      )),
    ])

  let events = vdom.init(prev)
  let diff = vdom.diff(0, prev, next, events)
  let handler = diff.events.handlers |> dict.get(0) |> should.be_ok

  decode.run(dynamic.from(Nil), handler) |> should.equal(Ok(10))
}
