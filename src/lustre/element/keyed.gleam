//// Lustre uses something called a _virtual DOM_ to work out what has changed
//// between renders and update the DOM accordingly. That means when you render
//// items in a list, Lustre will walk through the list of items and compare them
//// in order to see if they have changed.
////
//// This is often fine but it can be cause problems in cases where we'd like
//// Lustre to reuse existing DOM nodes more efficiently. Consider the example
//// in the [quickstart guide](../../guide/01-quickstart.html): each time the
//// counter is incremented, we insert a new image at the _start_ of the list.
////
//// Let's see how the virtual DOM handles this:
////
//// ```
//// Increment ->                 Increment ->
////              <img src="a">   -- update ->  <img src="b">
////                              -- insert ->  <img src="a">
//// ```
////
//// Beacuse the virtual DOM compares elements in order, it sees that the first
//// element has its `src` attribute changed from `"a"` to `"b"` and then sees
//// that a new element has been added to the _end_ of the list.
////
//// Intuitively, we know that what _really_ happened is that an element was
//// inserted at the _front_ of the list and ideally the first `<img />` should
//// be left untouched.
////
//// The solution is to assign a unique _key_ to each child element. This gives
//// Lustre enough information to reuse existing DOM nodes and avoid unnecessary
//// updates.
////
//// Keyed elements in Lustre work exactly like regular elements, but their child
//// list is a tuple of a unique key and the child itself:
////
//// ```gleam
//// keyed.div([], list.map(model.cats, fn(cat) {
////   #(cat.id, html.img([attribute.src(cat.url)]))
//// }))
//// ```
////
//// Let's see how the virtual DOM now handles this:
////
//// ```
//// Increment ->                 Increment ->
////                              -- insert ->  <img src="b">
////              <img href="a">  --        ->  <img src="a">
//// ```
////
//// We can see that Lustre has correctly recognised that the only change is a
//// new image being inserted at the front of the list. The first image is left
//// untouched!
////

// IMPORTS ---------------------------------------------------------------------

import gleam/list
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import lustre/internals/constants
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/vnode

// CONSTRUCTORS ----------------------------------------------------------------

/// Render a _keyed_ element with the given tag. Each child is assigned a unique
/// key, which Lustre uses to identify the element in the DOM. This is useful when
/// a single child can be moved around such as in a to-do list, or when elements
/// are frequently added or removed.
///
/// > **Note**: the key for each child must be unique within the list of children,
/// > but it doesn't have to be unique across the whole application. It's fine to
/// > use the same key in different lists.
///
pub fn element(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let #(keyed_children, children) = extract_keyed_children(children)

  vnode.element(
    key: "",
    namespace: "",
    tag:,
    attributes:,
    children:,
    keyed_children:,
  )
}

/// Render a _keyed_ element with the given namespace and tag. Each child is
/// assigned a unique key, which Lustre uses to identify the element in the DOM.
/// This is useful when a single child can be moved around such as in a to-do
/// list, or when elements are frequently added or removed.
///
/// > **Note**: the key for each child must be unique within the list of children,
/// > but it doesn't have to be unique across the whole application. It's fine to
/// > use the same key in different lists.
///
pub fn namespaced(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(#(String, Element(msg))),
) -> Element(msg) {
  let #(keyed_children, children) = extract_keyed_children(children)

  vnode.element(
    key: "",
    namespace:,
    tag:,
    attributes:,
    children:,
    keyed_children:,
  )
}

/// Render a _keyed_ fragment. Each child is assigned a unique key, which Lustre
/// uses to identify the element in the DOM. This is useful when a single child
/// can be moved around such as in a to-do list, or when elements are frequently
/// added or removed.
///
/// > **Note**: the key for each child must be unique within the list of children,
/// > but it doesn't have to be unique across the whole application. It's fine to
/// > use the same key in different lists.
///
pub fn fragment(children: List(#(String, Element(msg)))) -> Element(msg) {
  let #(keyed_children, children) = extract_keyed_children(children)

  vnode.fragment(key: "", children:, keyed_children:)
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
) -> #(MutableMap(String, Element(msg)), List(Element(msg))) {
  do_extract_keyed_children(children, mutable_map.new(), constants.empty_list)
}

fn do_extract_keyed_children(
  key_children_pairs: List(#(String, Element(msg))),
  keyed_children: MutableMap(String, Element(msg)),
  children: List(Element(msg)),
) -> #(MutableMap(String, Element(msg)), List(Element(msg))) {
  case key_children_pairs {
    [] -> #(keyed_children, list.reverse(children))

    [#(key, element), ..rest] -> {
      let keyed_element = vnode.to_keyed(key, element)

      // Children with empty keys are not inserted into the lookup, but they are
      // still returned in the children list.
      let keyed_children = case key {
        "" -> keyed_children
        _ -> mutable_map.insert(keyed_children, key, keyed_element)
      }
      let children = [keyed_element, ..children]

      do_extract_keyed_children(rest, keyed_children, children)
    }
  }
}
