// IMPORTS ---------------------------------------------------------------------

import agnostic/effect
import booklet
import gleam/dynamic
import lustre_test

// PERFORM TESTS ---------------------------------------------------------------

pub fn perform_drops_deferred_tasks_test() {
  use <- lustre_test.test_filter("perform_drops_deferred_tasks_test")

  let log = booklet.new([])
  let push = fn(entry) { booklet.update(log, fn(l) { [entry, ..l] }) }

  let effects =
    effect.batch([
      effect.from(fn(_dispatch) {
        push("sync")
        Nil
      }),
      effect.deferred("some_phase", fn(_dispatch, _root) {
        push("deferred")
        Nil
      }),
    ])

  effect.perform(
    effects,
    fn(_message) { Nil },
    fn(_name, _data) { Nil },
    fn(_selector) { Nil },
    fn() { dynamic.nil() },
    fn(_key, _value) { Nil },
    fn(_key, _decoder) { Nil },
    fn(_key) { Nil },
  )

  // `perform` runs only the synchronous tasks; tasks deferred to a platform
  // phase are dropped.
  assert booklet.get(log) == ["sync"]
}

pub fn perform_drops_mapped_deferred_tasks_test() {
  use <- lustre_test.test_filter("perform_drops_mapped_deferred_tasks_test")

  let log = booklet.new([])
  let push = fn(entry) { booklet.update(log, fn(l) { [entry, ..l] }) }

  let effects =
    effect.batch([
      effect.from(fn(dispatch) { dispatch("sync") }),
      effect.deferred("some_phase", fn(dispatch, _root) { dispatch("deferred") }),
    ])
    |> effect.map(fn(message) { "mapped:" <> message })

  effect.perform(
    effects,
    fn(message) {
      push(message)
      Nil
    },
    fn(_name, _data) { Nil },
    fn(_selector) { Nil },
    fn() { dynamic.nil() },
    fn(_key, _value) { Nil },
    fn(_key, _decoder) { Nil },
    fn(_key) { Nil },
  )

  assert booklet.get(log) == ["mapped:sync"]
}
