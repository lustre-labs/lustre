// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string
import lustre/internals/constants
import lustre/internals/vdom.{
  type Attribute, type Element, Attribute, Element, Event, Map, Text,
}

// TYPES -----------------------------------------------------------------------

pub type Patch(msg) {
  Diff(ElementDiff(msg))
  Emit(String, Json)
  Init(List(String), Element(msg))
}

pub type ElementDiff(msg) {
  ElementDiff(
    created: Dict(String, Element(msg)),
    removed: Set(String),
    updated: Dict(String, AttributeDiff(msg)),
    handlers: Dict(String, Decoder(msg)),
  )
}

pub type AttributeDiff(msg) {
  AttributeDiff(
    created: Set(Attribute(msg)),
    removed: Set(String),
    handlers: Dict(String, Decoder(msg)),
  )
}

// COMPUTING DIFFS -------------------------------------------------------------

pub fn elements(old: Element(msg), new: Element(msg)) -> ElementDiff(msg) {
  do_elements(
    ElementDiff(dict.new(), set.new(), dict.new(), dict.new()),
    Some(old),
    Some(new),
    "0",
  )
}

fn do_elements(
  diff: ElementDiff(msg),
  old: Option(Element(msg)),
  new: Option(Element(msg)),
  key: String,
) -> ElementDiff(msg) {
  case old, new {
    None, None -> diff
    Some(_), None -> ElementDiff(..diff, removed: set.insert(diff.removed, key))
    None, Some(new) ->
      ElementDiff(
        ..diff,
        created: dict.insert(diff.created, key, new),
        handlers: fold_event_handlers(diff.handlers, new, key),
      )

    Some(old), Some(new) -> {
      case old, new {
        Map(old_subtree), Map(new_subtree) ->
          do_elements(diff, Some(old_subtree()), Some(new_subtree()), key)
        Map(subtree), _ -> do_elements(diff, Some(subtree()), Some(new), key)
        _, Map(subtree) -> do_elements(diff, Some(old), Some(subtree()), key)
        Text(old), Text(new) if old == new -> diff
        // We have two text nodes but their text content is not the same. We could
        // be *really* granular here and compute a diff of the text content itself
        // but we're not going to gain much from that.
        Text(_), Text(_) ->
          ElementDiff(..diff, created: dict.insert(diff.created, key, new))

        // We previously had an element node but now we have a text node. All we
        // need to do is mark the new one as created and it will replace the old
        // element during patching.
        Element(_, _, _, _, _, _, _), Text(_) ->
          ElementDiff(..diff, created: dict.insert(diff.created, key, new))

        Text(_), Element(_, _, _, _, _, _, _) as new ->
          ElementDiff(
            ..diff,
            created: dict.insert(diff.created, key, new),
            handlers: fold_event_handlers(diff.handlers, new, key),
          )

        // For two elements to be diffed rather than replaced, it is necessary
        // for both their namespaces and their tags to be the same. If that is
        // the case, we can dif their attributes to see what (if anything) has
        // changed, and then recursively diff their children.
        Element(_, old_ns, old_tag, old_attrs, old_children, _, _), Element(
            _,
            new_ns,
            new_tag,
            new_attrs,
            new_children,
            _,
            _,
          ) if old_ns == new_ns && old_tag == new_tag -> {
          let attribute_diff = attributes(old_attrs, new_attrs)
          let handlers = {
            use handlers, name, handler <- dict.fold(
              attribute_diff.handlers,
              diff.handlers,
            )

            let name = string.drop_left(name, 2)
            dict.insert(handlers, key <> "-" <> name, handler)
          }
          let diff =
            ElementDiff(
              ..diff,
              updated: case is_empty_attribute_diff(attribute_diff) {
                True -> diff.updated
                False -> dict.insert(diff.updated, key, attribute_diff)
              },
              handlers: handlers,
            )

          // This local `zip` function takes two lists of potentially different
          // sizes and zips them together, padding the shorter list with `None`.
          let children = zip(old_children, new_children)
          use diff, #(old, new), pos <- list.index_fold(children, diff)
          let key = key <> "-" <> int.to_string(pos)

          do_elements(diff, old, new, key)
        }

        // When we have two elements, but their namespaces or their tags differ,
        // there is nothing to diff. We mark the new element as created and
        // extract any event handlers.
        Element(_, _, _, _, _, _, _), Element(_, _, _, _, _, _, _) as new ->
          ElementDiff(
            ..diff,
            created: dict.insert(diff.created, key, new),
            handlers: fold_event_handlers(diff.handlers, new, key),
          )
      }
    }
  }
}

pub fn attributes(
  old: List(Attribute(msg)),
  new: List(Attribute(msg)),
) -> AttributeDiff(msg) {
  let old = attribute_dict(old)
  let new = attribute_dict(new)
  let init = AttributeDiff(set.new(), set.new(), dict.new())

  let #(diff, new) = {
    use #(diff, new), key, attr <- dict.fold(old, #(init, new))
    let new_attr = dict.get(new, key)
    let diff = do_attribute(diff, key, Ok(attr), new_attr)
    let new = dict.delete(new, key)

    #(diff, new)
  }

  // Once we've diffed all the old attributes, all that's left is any remaining
  // new attributes to add.
  use diff, key, attr <- dict.fold(new, diff)
  do_attribute(diff, key, Error(Nil), Ok(attr))
}

fn do_attribute(
  diff: AttributeDiff(msg),
  key: String,
  old: Result(Attribute(msg), Nil),
  new: Result(Attribute(msg), Nil),
) -> AttributeDiff(msg) {
  case old, new {
    Error(_), Error(_) -> diff
    Ok(old), Ok(Event(name, handler) as new) if old == new ->
      AttributeDiff(..diff, handlers: dict.insert(diff.handlers, name, handler))
    Ok(old), Ok(new) if old == new -> diff
    Ok(_), Error(_) ->
      AttributeDiff(..diff, removed: set.insert(diff.removed, key))

    // It's not until JSON encoding that these event handlers will be converted
    // to normal attributes. That's intentional in case we want to do anything
    // with this diff _besides_ serialise it in the future.
    _, Ok(Event(name, handler) as new) ->
      AttributeDiff(
        ..diff,
        created: set.insert(diff.created, new),
        handlers: dict.insert(diff.handlers, name, handler),
      )

    _, Ok(new) -> AttributeDiff(..diff, created: set.insert(diff.created, new))
  }
}

// CONVERSIONS -----------------------------------------------------------------

pub fn patch_to_json(patch: Patch(msg)) -> Json {
  case patch {
    Diff(diff) ->
      json.preprocessed_array([
        json.int(constants.diff),
        element_diff_to_json(diff),
      ])
    Emit(name, event) ->
      json.preprocessed_array([
        json.int(constants.emit),
        json.string(name),
        event,
      ])
    Init(attrs, element) ->
      json.preprocessed_array([
        json.int(constants.init),
        json.array(attrs, json.string),
        vdom.element_to_json(element),
      ])
  }
}

pub fn element_diff_to_json(diff: ElementDiff(msg)) -> Json {
  json.preprocessed_array([
    json.preprocessed_array({
      use array, key, element <- dict.fold(diff.created, [])
      let json =
        json.preprocessed_array([
          json.string(key),
          vdom.element_to_json(element),
        ])

      [json, ..array]
    }),
    json.preprocessed_array({
      use array, key <- set.fold(diff.removed, [])
      let json = json.preprocessed_array([json.string(key)])

      [json, ..array]
    }),
    json.preprocessed_array({
      use array, key, diff <- dict.fold(diff.updated, [])
      use <- bool.guard(is_empty_attribute_diff(diff), array)

      let json =
        json.preprocessed_array([
          json.string(key),
          attribute_diff_to_json(diff, key),
        ])

      [json, ..array]
    }),
  ])
}

pub fn attribute_diff_to_json(diff: AttributeDiff(msg), key: String) -> Json {
  json.preprocessed_array([
    json.preprocessed_array({
      use array, attr <- set.fold(diff.created, [])
      case vdom.attribute_to_json(attr, key) {
        Ok(json) -> [json, ..array]
        Error(_) -> array
      }
    }),
    json.preprocessed_array({
      use array, key <- set.fold(diff.removed, [])
      [json.string(key), ..array]
    }),
  ])
}

// UTILS -----------------------------------------------------------------------

fn zip(xs: List(a), ys: List(a)) -> List(#(Option(a), Option(a))) {
  case xs, ys {
    [], [] -> []
    [x, ..xs], [y, ..ys] -> [#(Some(x), Some(y)), ..zip(xs, ys)]
    [x, ..xs], [] -> [#(Some(x), None), ..zip(xs, [])]
    [], [y, ..ys] -> [#(None, Some(y)), ..zip([], ys)]
  }
}

// For diffing attributes, it is much easier if we have a `Dict` to work with
// rather than two lists. This function takes an attribute list and converts it
// to a dictionary. Repeated attribute keys are *replaced* as the dict is built,
// with the exception of `class` and `style` attributes which are *merged*.
//
// This special merging behaviour is necessary to preserve the runtime semantics
// of Lustre's client patching.
fn attribute_dict(
  attributes: List(Attribute(msg)),
) -> Dict(String, Attribute(msg)) {
  use dict, attr <- list.fold(attributes, dict.new())

  case attr {
    Attribute("class", value, _) ->
      case dict.get(dict, "class") {
        Ok(Attribute(_, classes, _)) -> {
          let classes =
            dynamic.from(
              dynamic.unsafe_coerce(classes)
              <> " "
              <> dynamic.unsafe_coerce(value),
            )
          dict.insert(dict, "class", Attribute("class", classes, False))
        }

        Ok(_) | Error(_) -> dict.insert(dict, "class", attr)
      }

    Attribute("style", value, _) ->
      case dict.get(dict, "style") {
        Ok(Attribute(_, styles, _)) -> {
          let styles =
            dynamic.from(list.append(
              dynamic.unsafe_coerce(styles),
              dynamic.unsafe_coerce(value),
            ))
          dict.insert(dict, "style", Attribute("style", styles, False))
        }
        Ok(_) | Error(_) -> dict.insert(dict, "class", attr)
      }

    Attribute(key, _, _) -> dict.insert(dict, key, attr)
    Event(key, _) -> dict.insert(dict, key, attr)
  }
}

fn event_handler(
  attribute: Attribute(msg),
) -> Result(#(String, Decoder(msg)), Nil) {
  case attribute {
    Attribute(_, _, _) -> Error(Nil)
    Event(name, handler) -> {
      let name = string.drop_left(name, 2)

      Ok(#(name, handler))
    }
  }
}

fn fold_event_handlers(
  handlers: Dict(String, Decoder(msg)),
  element: Element(msg),
  key: String,
) -> Dict(String, Decoder(msg)) {
  case element {
    Text(_) -> handlers
    Map(subtree) -> fold_event_handlers(handlers, subtree(), key)
    Element(_, _, _, attrs, children, _, _) -> {
      let handlers =
        list.fold(attrs, handlers, fn(handlers, attr) {
          case event_handler(attr) {
            Ok(#(name, handler)) -> {
              let name = string.drop_left(name, 2)
              dict.insert(handlers, key <> "-" <> name, handler)
            }
            Error(_) -> handlers
          }
        })
      use handlers, child, index <- list.index_fold(children, handlers)
      let key = key <> "-" <> int.to_string(index)

      fold_event_handlers(handlers, child, key)
    }
  }
}

pub fn is_empty_element_diff(diff: ElementDiff(msg)) -> Bool {
  diff.created == dict.new()
  && diff.removed == set.new()
  && diff.updated == dict.new()
}

fn is_empty_attribute_diff(diff: AttributeDiff(msg)) -> Bool {
  diff.created == set.new() && diff.removed == set.new()
}
