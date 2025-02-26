// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type DecodeError, type Decoder}
import gleam/io
import gleam/list
import gleam/result
import lustre/internals/constants

// TYPES -----------------------------------------------------------------------

///
///
pub type Event(msg) {
  Event(
    mappers: List(fn(Dynamic) -> Dynamic),
    // NOTE: this type variable is fake, and only true once you apply all the mappers!
    handler: Decoder(msg),
  )
}

///
///
pub type Events(msg) {
  Events(
    events: Dict(Int, Event(msg)),
    entries: Dict(Event(msg), Entry),
    next_id: Int,
  )
}

pub type Entry {
  Entry(id: Int, reference_count: Int)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn new() -> Events(msg) {
  Events(
    events: constants.empty_dict(),
    entries: constants.empty_dict(),
    next_id: 0,
  )
}

///
///
pub fn reset(events: Events(msg)) -> Events(msg) {
  Events(..events, events: constants.empty_dict())
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn get(events: Events(msg), event: Event(msg)) -> Int {
  case dict.get(events.entries, event) {
    Ok(Entry(id:, ..)) -> id
    _ -> {
      io.debug(#("NOT FOUND", event, events.entries))
      -1
    }
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn replace(
  events: Events(msg),
  prev: Event(msg),
  next: Event(msg),
) -> Events(msg) {
  case dict.get(events.entries, prev) {
    Ok(Entry(id:, ..)) ->
      case dict.has_key(events.events, id) {
        False ->
          // remove old and insert new with same id -
          // we set the reference count to 1 here because it isn't guaranteed
          // that the structure of the new tree matches the old tree!
          // If we get the same next event in a replace call again, we
          // increase the reference_count in insert as usual.
          Events(
            events: dict.insert(events.events, id, next),
            entries: events.entries
              |> dict.delete(prev)
              |> dict.insert(next, Entry(id:, reference_count: 1)),
            next_id: events.next_id,
          )

        True ->
          // this id has already been used and we can't replace it!
          insert(events, next)
      }

    Error(_) -> insert(events, next)
  }
}

///
///
pub fn insert(events: Events(msg), event: Event(msg)) -> Events(msg) {
  case dict.get(events.entries, event) {
    // event already exists - increase reference count
    Ok(Entry(id:, reference_count:)) -> {
      let entry = Entry(id:, reference_count: reference_count + 1)
      Events(..events, entries: dict.insert(events.entries, event, entry))
    }

    // register as a new event.
    Error(_) -> {
      let entry = Entry(id: events.next_id, reference_count: 1)
      Events(
        events: dict.insert(events.events, events.next_id, event),
        entries: dict.insert(events.entries, event, entry),
        next_id: events.next_id + 1,
      )
    }
  }
}

///
///
pub fn forget(events: Events(msg), event: Event(msg)) -> Events(msg) {
  case dict.get(events.entries, event) {
    Ok(Entry(id:, reference_count:)) if reference_count > 1 -> {
      let entry = Entry(id:, reference_count: reference_count - 1)
      Events(..events, entries: dict.insert(events.entries, event, entry))
    }

    Ok(Entry(id:, ..)) ->
      // once the reference count hits 1, we still have to check whether or not
      // we used this event during this render already
      case dict.has_key(events.events, id) {
        False -> Events(..events, entries: dict.delete(events.entries, event))
        True -> events
      }

    Error(_) -> events
  }
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn run(
  events: Events(msg),
  id: Int,
  event: Dynamic,
) -> Result(msg, List(DecodeError)) {
  case dict.get(events.events, id) {
    Ok(Event(mappers:, handler:)) -> {
      use decoded <- result.map(decode.run(event, handler))
      use mapped, mapper <- list.fold(mappers, decoded)
      coerce(mapper(dynamic.from(mapped)))
    }
    Error(_) -> Error([])
  }
}

// FFI -------------------------------------------------------------------------

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b
