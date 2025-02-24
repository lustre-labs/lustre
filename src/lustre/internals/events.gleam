// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type DecodeError, type Decoder}
import lustre/internals/constants

// TYPES -----------------------------------------------------------------------

///
///
pub type Events(msg) {
  Events(
    handlers: Dict(Int, Decoder(msg)),
    ids: Dict(Decoder(msg), Int),
    next_id: Int,
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn new() -> Events(msg) {
  Events(
    handlers: constants.empty_dict(),
    ids: constants.empty_dict(),
    next_id: 0,
  )
}

///
///
pub fn reset(events: Events(msg)) -> Events(msg) {
  Events(
    handlers: constants.empty_dict(),
    ids: events.ids,
    next_id: events.next_id,
  )
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn get(events: Events(msg), handler: Decoder(msg)) -> Result(Int, Nil) {
  dict.get(events.ids, handler)
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn replace(
  events: Events(msg),
  prev: Decoder(msg),
  next: Decoder(msg),
) -> #(Int, Events(msg)) {
  case dict.get(events.ids, prev) {
    Ok(id) ->
      case prev == next {
        True -> #(id, events)
        False -> #(
          id,
          Events(
            handlers: dict.insert(events.handlers, id, next),
            ids: dict.insert(events.ids, next, id),
            next_id: events.next_id,
          ),
        )
      }

    Error(_) -> #(
      events.next_id,
      Events(
        handlers: dict.insert(events.handlers, events.next_id, next),
        ids: dict.insert(events.ids, next, events.next_id),
        next_id: events.next_id + 1,
      ),
    )
  }
}

///
///
pub fn insert(events: Events(msg), handler: Decoder(msg)) -> #(Int, Events(msg)) {
  #(
    events.next_id,
    Events(
      handlers: dict.insert(events.handlers, events.next_id, handler),
      ids: dict.insert(events.ids, handler, events.next_id),
      next_id: events.next_id + 1,
    ),
  )
}

///
///
pub fn forget(events: Events(msg), handler: Decoder(msg)) -> Events(msg) {
  Events(
    handlers: events.handlers,
    ids: dict.delete(events.ids, handler),
    next_id: events.next_id,
  )
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn run(
  events: Events(msg),
  id: Int,
  event: Dynamic,
) -> Result(msg, List(DecodeError)) {
  case dict.get(events.handlers, id) {
    Ok(decoder) -> decode.run(event, decoder)
    Error(_) -> Error([])
  }
}
