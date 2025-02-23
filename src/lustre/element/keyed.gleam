// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/list
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import lustre/internals/constants
import lustre/runtime/vdom.{Fragment, Node, Text}

// CONSTRUCTORS ----------------------------------------------------------------

/// Keying elements is an optimisation that helps the runtime reuse existing DOM
/// nodes in cases where children are reordered or removed from a list. Maybe you
/// have a list of elements that can be filtered or sorted in some way, or additions
/// to the front are common. In these cases, keying elements can help Lustre avoid
/// unecessary DOM manipulations by pairing the DOM nodes with the elements in the
/// list that share the same key.
///
/// You can easily take an element from `lustre/element/html` and key its children
/// by making use of Gleam's [function capturing syntax](https://tour.gleam.run/functions/function-captures/):
///
/// ```gleam
/// import gleam/list
/// import lustre/element
/// import lustre/element/html
///
/// fn example() {
///   element.keyed(html.ul([], _), {
///     use item <- list.map(todo_list)
///     let child = html.li([], [view_item(item)])
///
///     #(item.id, child)
///   })
/// }
/// ```
///
/// **Note**: The key must be unique within the list of children, but it doesn't
/// have to be unique across the whole application. It's fine to use the same key
/// in different lists. Lustre will display a warning in the browser console when
/// it detects duplicate keys in a list.
///
pub fn element(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let #(keyed_children, children) = extract_keyed_children(children)

  Node(
    key: "",
    namespace: "",
    tag:,
    attributes: vdom.sort_attributes(attributes),
    children:,
    keyed_children:,
    self_closing: False,
    void: False,
  )
}

pub fn namespaced(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let #(keyed_children, children) = extract_keyed_children(children)

  Node(
    key: "",
    namespace:,
    tag:,
    attributes: vdom.sort_attributes(attributes),
    children:,
    keyed_children:,
    self_closing: False,
    void: False,
  )
}

pub fn fragment(children: List(#(String, Element(msg)))) -> Element(msg) {
  let #(keyed_children, children) = extract_keyed_children(children)

  Fragment(
    key: "",
    children:,
    children_count: dict.size(keyed_children),
    keyed_children:,
  )
}

// ELEMENTS --------------------------------------------------------------------

pub fn ul(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  element("ul", attributes, children)
}

pub fn ol(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  element("ol", attributes, children)
}

pub fn div(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  element("div", attributes, children)
}

pub fn tbody(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  element("tbody", attributes, children)
}

pub fn dl(
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  element("dl", attributes, children)
}

// UTILS -----------------------------------------------------------------------

fn extract_keyed_children(
  children: List(#(String, Element(msg))),
) -> #(Dict(String, Element(msg)), List(Element(msg))) {
  list.map_fold(children, constants.empty_dict(), fn(keyed_children, child) {
    let key = child.0
    let element = key_element(key, child.1)

    #(dict.insert(keyed_children, key, element), element)
  })
}

fn key_element(key: String, element: Element(msg)) -> Element(msg) {
  case element {
    Fragment(..) -> Fragment(..element, key:)
    Node(..) -> Node(..element, key:)
    Text(..) -> Text(..element, key:)
  }
}
