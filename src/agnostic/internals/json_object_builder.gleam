//// This module contains helper functions for building up json objects using
//// pipelines. This allows us to skip "empty" values, reducing payload size.
////
//// It builds up a list of entries in reverse; but since we do not care about
//// the order of our keys, we can just leave the list like it is.

import gleam/json.{type Json}
import lustre/internals/constants

pub type Builder =
  List(#(String, Json))

pub fn new() -> Builder {
  constants.empty_list
}

pub fn tagged(kind: Int) -> Builder {
  [#("kind", json.int(kind))]
}

pub fn build(entries: Builder) -> Json {
  json.object(entries)
}

pub fn json(entries: Builder, key: String, value: Json) -> Builder {
  [#(key, value), ..entries]
}

pub fn string(entries: Builder, key: String, value: String) -> Builder {
  case value != "" {
    True -> [#(key, json.string(value)), ..entries]
    False -> entries
  }
}

pub fn int(entries: Builder, key: String, value: Int) -> Builder {
  case value != 0 {
    True -> [#(key, json.int(value)), ..entries]
    False -> entries
  }
}

pub fn bool(entries: Builder, key: String, value: Bool) -> Builder {
  case value {
    True -> [#(key, json.int(1)), ..entries]
    False -> entries
  }
}

pub fn list(
  entries: Builder,
  key: String,
  values: List(a),
  to_json: fn(a) -> Json,
) -> Builder {
  case values {
    [] -> entries
    _ -> [#(key, json.array(values, to_json)), ..entries]
  }
}

pub fn object(entries: Builder, key: String, nested: Builder) -> Builder {
  case nested {
    [] -> entries
    _ -> [#(key, json.object(nested)), ..entries]
  }
}
