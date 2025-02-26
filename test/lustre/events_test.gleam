import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/int
import gleeunit/should
import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/element/keyed
import lustre/internals/events
import lustre/runtime/vdom
import lustre_test

// EDGE CASES ------------------------------------------------------------------

pub fn duplicated_handler_removed_test() {
  use <- lustre_test.test_filter("duplicated_handler_removed_test")

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
      html.button([click], [html.text("Click me!")]),
    ])

  let prev_events = vdom.init(prev)
  let event = events.Event([], click.handler)
  let prev_entry = events.Entry(id: 0, reference_count: 2)
  prev_events.events |> dict.get(0) |> should.equal(Ok(event))
  prev_events.entries |> dict.get(event) |> should.equal(Ok(prev_entry))

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let diff = vdom.diff(0, prev, next, prev_events)

  let new_entry = events.Entry(id: 0, reference_count: 1)
  diff.events.events |> dict.get(0) |> should.equal(Ok(event))
  diff.events.entries |> dict.get(event) |> should.equal(Ok(new_entry))
}

pub fn duplicated_handler_mapped_test() {
  use <- lustre_test.test_filter("duplicated_handler_mapped_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let mapper = int.add(_, 2)

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
      element.map(html.button([click], [html.text("Click me!")]), mapper),
    ])

  let events = vdom.init(prev)

  let event1 = events.Event([], click.handler)
  let event2 = events.Event([coerce(mapper)], click.handler)

  events.events |> dict.get(0) |> should.equal(Ok(event1))
  events.entries |> dict.get(event1) |> should.equal(Ok(events.Entry(0, 1)))

  events.events |> dict.get(1) |> should.equal(Ok(event2))
  events.entries |> dict.get(event2) |> should.equal(Ok(events.Entry(1, 1)))

  events.run(events, 0, dynamic.from(Nil))
  |> should.equal(Ok(1))

  events.run(events, 1, dynamic.from(Nil))
  |> should.equal(Ok(3))
}

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
  let event = events.Event([], click.handler)

  events.events |> dict.get(0) |> should.equal(Ok(event))
  events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))
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
  let event1 = events.Event([], click.handler)
  let event2 = events.Event([], keydown.handler)

  events.events |> dict.get(0) |> should.equal(Ok(event1))
  events.entries |> dict.get(event1) |> should.equal(Ok(events.Entry(0, 1)))

  events.events |> dict.get(1) |> should.equal(Ok(event2))
  events.entries |> dict.get(event2) |> should.equal(Ok(events.Entry(1, 1)))
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
  let event = events.Event([], click.handler)

  prev_events |> should.equal(events.new())
  diff.events.events |> dict.get(0) |> should.equal(Ok(event))
  diff.events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))
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

  let event1 = events.Event([], click.handler)
  let event2 = events.Event([], keydown.handler)

  diff.events.events |> dict.get(0) |> should.equal(Ok(event1))
  diff.events.entries
  |> dict.get(event1)
  |> should.equal(Ok(events.Entry(0, 1)))

  prev_events.events |> dict.get(1) |> should.equal(Ok(event2))
  prev_events.entries
  |> dict.get(event2)
  |> should.equal(Ok(events.Entry(1, 1)))

  diff.events.events |> dict.get(1) |> should.equal(Error(Nil))
  diff.events.entries |> dict.get(event2) |> should.equal(Error(Nil))
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

  let event1 = events.Event([], click.handler)
  let event2 = events.Event([], keydown.handler)

  prev_events.events |> dict.get(0) |> should.equal(Ok(event1))
  prev_events.entries
  |> dict.get(event1)
  |> should.equal(Ok(events.Entry(0, 1)))

  diff.events.events |> dict.get(0) |> should.equal(Error(Nil))
  diff.events.entries |> dict.get(event1) |> should.equal(Error(Nil))

  diff.events.events |> dict.get(1) |> should.equal(Ok(event2))
  diff.events.entries
  |> dict.get(event2)
  |> should.equal(Ok(events.Entry(1, 1)))
}

pub fn diff_added_event_at_start_test() {
  use <- lustre_test.test_filter("diff_added_event_at_start_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev = html.button([attribute.type_("button")], [html.text("Click me!")])
  let prev_events = vdom.init(prev)

  let next =
    html.button([click, attribute.type_("button")], [html.text("Click me!")])

  let diff = vdom.diff(0, prev, next, prev_events)
  let event = events.Event([], click.handler)

  diff.events.events |> dict.get(0) |> should.equal(Ok(event))
  diff.events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))
}

pub fn diff_added_event_at_end_test() {
  use <- lustre_test.test_filter("diff_added_event_at_end_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev = html.button([attribute.class("btn")], [html.text("Click me!")])
  let prev_events = vdom.init(prev)

  let next =
    html.button([click, attribute.class("btn")], [html.text("Click me!")])

  let diff = vdom.diff(0, prev, next, prev_events)
  let event = events.Event([], click.handler)

  diff.events.events |> dict.get(0) |> should.equal(Ok(event))
  diff.events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))
}

pub fn diff_added_event_at_middle_test() {
  use <- lustre_test.test_filter("diff_added_event_at_middle_test")

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
    html.button([attribute.class("btn"), attribute.type_("button")], [
      html.text("Click me!"),
    ])
  let prev_events = vdom.init(prev)

  let next =
    html.button([click, attribute.class("btn"), attribute.type_("button")], [
      html.text("Click me!"),
    ])

  let diff = vdom.diff(0, prev, next, prev_events)
  let event = events.Event([], click.handler)

  diff.events.events |> dict.get(0) |> should.equal(Ok(event))
  diff.events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))
}

pub fn diff_removed_event_at_middle_test() {
  use <- lustre_test.test_filter("diff_removed_event_at_middle_test")

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
    html.button([click, attribute.class("btn"), attribute.type_("button")], [
      html.text("Click me!"),
    ])
  let prev_events = vdom.init(prev)

  let next =
    html.button([attribute.class("btn"), attribute.type_("button")], [
      html.text("Click me!"),
    ])

  let diff = vdom.diff(0, prev, next, prev_events)
  let event = events.Event([], click.handler)

  prev_events.events |> dict.get(0) |> should.equal(Ok(event))
  prev_events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))

  diff.events.events |> dict.get(0) |> should.equal(Error(Nil))
  diff.events.entries |> dict.get(event) |> should.equal(Error(Nil))
}

pub fn diff_replace_event_with_attribute_test() {
  use <- lustre_test.test_filter("diff_replace_event_with_attribute_test")

  let open =
    vdom.Event(
      name: "open",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev = html.details([open], [html.text("Wibble")])
  let next =
    html.details([attribute.attribute("open", "")], [html.text("Wibble")])

  let prev_events = vdom.init(prev)
  let diff = vdom.diff(0, prev, next, prev_events)
  let event = events.Event([], open.handler)

  let patch =
    vdom.Patch(0, 0, [], [
      vdom.Patch(
        0,
        0,
        [vdom.Update([attribute.attribute("open", "")], [open])],
        [],
      ),
    ])

  prev_events.events |> dict.get(0) |> should.equal(Ok(event))
  prev_events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))

  diff.events.events |> dict.get(0) |> should.be_error
  diff.events.entries |> dict.get(event) |> should.be_error

  diff.patch |> should.equal(patch)
}

pub fn diff_replace_attribute_with_event_test() {
  use <- lustre_test.test_filter("diff_replace_attribute_with_event_test")

  let open =
    vdom.Event(
      name: "open",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let prev =
    html.details([attribute.attribute("open", "")], [html.text("Wibble")])
  let next = html.details([open], [html.text("Wibble")])

  let diff = vdom.diff(0, prev, next, events.new())
  let event = events.Event([], open.handler)

  let patch =
    vdom.Patch(0, 0, [], [
      vdom.Patch(
        0,
        0,
        [vdom.Update([open], [attribute.attribute("open", "")])],
        [],
      ),
    ])

  diff.events.events |> dict.get(0) |> should.equal(Ok(event))
  diff.events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))

  diff.patch |> should.equal(patch)
}

pub fn diff_keyed_move_with_events_test() {
  use <- lustre_test.test_filter("diff_keyed_move_with_events_test")

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
    keyed.div([], [
      #("0", html.text("first")),
      #("1", html.button([click], [html.text("Click me!")])),
    ])

  let next =
    keyed.div([], [#("1", html.button([click], [html.text("Click me!")]))])

  let prev_events = vdom.init(prev)
  let diff = vdom.diff(0, prev, next, prev_events)

  let event = events.Event([], click.handler)
  diff.events.events |> dict.get(0) |> should.equal(Ok(event))
  diff.events.entries |> dict.get(event) |> should.equal(Ok(events.Entry(0, 1)))
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

  let mapper = int.add(_, 2)

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), mapper),
    ])

  let events = vdom.init(prev)
  let event = events.Event([coerce(mapper)], click.handler)

  events.events |> dict.get(0) |> should.equal(Ok(event))
  events.run(events, 0, dynamic.from(Nil))
  |> should.equal(Ok(3))
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

  let mapper1 = int.add(_, 2)
  let mapper2 = int.multiply(_, 2)

  let prev =
    element.map(
      html.div([], [
        html.h1([], [html.text("Hello, Joe!")]),
        element.map(html.button([click], [html.text("Click me!")]), mapper1),
      ]),
      mapper2,
    )

  let events = vdom.init(prev)
  let event = events.Event([coerce(mapper1), coerce(mapper2)], click.handler)

  events.events |> dict.get(0) |> should.equal(Ok(event))
  events.run(events, 0, dynamic.from(Nil)) |> should.equal(Ok(6))
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

  let mapper = int.add(_, 2)

  let prev =
    element.map(
      element.fragment([html.button([click], [html.text("Click me!")])]),
      mapper,
    )

  let events = vdom.init(prev)
  let event = events.Event([coerce(mapper)], click.handler)

  events.events |> dict.get(0) |> should.equal(Ok(event))
  events.run(events, 0, dynamic.from(Nil)) |> should.equal(Ok(3))
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

  let mapper = int.add(_, 2)
  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), mapper),
    ])

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      html.button([click], [html.text("Click me!")]),
    ])

  let prev_events = vdom.init(prev)
  let diff = vdom.diff(0, prev, next, prev_events)

  events.run(prev_events, 0, dynamic.from(Nil)) |> should.equal(Ok(3))
  events.run(diff.events, 0, dynamic.from(Nil)) |> should.equal(Ok(1))
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

  let prev_events = vdom.init(prev)
  let diff = vdom.diff(0, prev, next, prev_events)

  events.run(prev_events, 0, dynamic.from(Nil)) |> should.equal(Ok(3))
  events.run(diff.events, 0, dynamic.from(Nil)) |> should.equal(Ok(10))
}

pub fn event_mapper_split_test() {
  use <- lustre_test.test_filter("event_mapper_split_test")

  let click =
    vdom.Event(
      name: "click",
      handler: decode.success(1),
      include: [],
      prevent_default: False,
      stop_propagation: False,
      immediate: False,
    )

  let mapper1 = int.add(_, 2)
  let mapper2 = int.multiply(_, 10)

  let prev =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), mapper1),
      element.map(html.button([click], [html.text("Click me!")]), mapper1),
    ])

  let next =
    html.div([], [
      html.h1([], [html.text("Hello, Joe!")]),
      element.map(html.button([click], [html.text("Click me!")]), mapper1),
      element.map(html.button([click], [html.text("Click me!")]), mapper2),
    ])

  let prev_events = vdom.init(prev)
  let diff = vdom.diff(0, prev, next, prev_events)

  events.run(prev_events, 0, dynamic.from(Nil)) |> should.equal(Ok(3))
  events.run(diff.events, 0, dynamic.from(Nil)) |> should.equal(Ok(3))
  events.run(diff.events, 1, dynamic.from(Nil)) |> should.equal(Ok(10))
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b
