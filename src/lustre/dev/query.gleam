// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/order
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/internals/constants
import lustre/vdom/path.{type Path}
import lustre/vdom/vattr.{Attribute}
import lustre/vdom/vnode.{Element, Fragment, Map, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

/// A `Query` that describes how to locate certain elements in an `Element` tree.
/// You can pass a `Query` to functions like [`find`](#find) and [`find_all`](#find_all)
/// write tests that assert certain elements are present in your views.
///
pub opaque type Query {
  FindElement(matching: Selector)
  FindChild(of: Query, matching: Selector)
  FindDescendant(of: Query, matching: Selector)
}

/// A `Selector` describes how to match a specific element in an `Element` tree.
/// It might be the element's tag name, a class name, an attribute, or some
/// combination of these.
///
pub opaque type Selector {
  All(of: List(Selector))
  Type(namespace: String, tag: String)
  HasAttribute(name: String, value: String)
  HasClass(name: String)
  HasStyle(name: String, value: String)
  Contains(content: String)
}

// QUERIES ---------------------------------------------------------------------

/// Find any elements in a view that match the given [`Selector`](#Selector).
///
pub fn element(matching selector: Selector) -> Query {
  FindElement(matching: selector)
}

/// Given a `Query` that finds an element, find any of that element's _direct_
/// children that match the given [`Selector`](#Selector). This is similar to the
/// CSS `>` combinator.
///
pub fn child(of parent: Query, matching selector: Selector) -> Query {
  FindChild(of: parent, matching: selector)
}

/// Given a `Query` that finds an element, find any of that element's _descendants_
/// that match the given [`Selector`](#Selector). This will walk the entire tree
/// from the matching parent.
///
pub fn descendant(of parent: Query, matching selector: Selector) -> Query {
  FindDescendant(of: parent, matching: selector)
}

// SELECTORS -------------------------------------------------------------------

/// Combine two selectors into one that must match both. For example, if you have
/// a selector for div elements and a selector for elements with the class "wibble"
/// then they can be combined into a selector that matches only div elements with
/// the class "wibble".
///
/// ```gleam
/// import lustre/dev/query
///
/// pub fn example() {
///   let div = query.tag("div")
///   let wibble = query.class("wibble")
///
///   query.element(matching: div |> query.and(wibble))
/// }
/// ```
///
/// You can chain multiple `and` calls together to combine many selectors into
/// something more specific.
///
/// ```gleam
/// import lustre/dev/query
///
/// pub fn example() {
///   query.tag("div")
///   |> query.and(query.class("wibble"))
///   |> query.and(query.data("open", "true"))
/// }
/// ```
///
/// > **Note**: if you find yourself crafting complex selectors, consider using
/// > a test id on the element(s) you want to find instead.
///
pub fn and(first: Selector, second: Selector) -> Selector {
  case first {
    All(of: []) -> All(of: [second])
    All(of: others) -> All(of: [second, ..others])
    _ -> All(of: [first, second])
  }
}

/// Select elements based on their tag name, like `"div"`, `"span"`, or `"a"`.
/// To select elements with an XML namespace - such as SVG elements - use the
/// [`namespaced`](#namespaced) selector instead.
///
pub fn tag(value: String) -> Selector {
  Type(namespace: "", tag: value)
}

/// Select elements based on their tag name and XML namespace. This is useful
/// for selecting SVG elements or other XML elements that have a namespace.
/// For example, to select an SVG circle element, you would use:
///
/// ```gleam
/// import lustre/dev/query
///
/// pub fn example() {
///   let svg = "http://www.w3.org/2000/svg"
///
///   query.element(matching: query.namespaced(svg, "circle"))
/// }
/// ```
///
pub fn namespaced(namespace: String, tag: String) -> Selector {
  Type(namespace:, tag:)
}

/// Select elements that have the specified attribute with the given value. If
/// the value is left blank, this selector will match any element that has the
/// attribute, _regardless of its value_.
///
/// For example, to select a form input with the name "username", you would
/// use:
///
/// ```gleam
/// import lustre/dev/query
///
/// pub fn example() {
///   query.element(matching: query.attribute("name", "username"))
/// }
/// ```
///
/// Or to select elements with the `disabled` attribute:
///
/// ```gleam
/// import lustre/dev/query
///
/// pub fn example() {
///   query.element(matching: query.attribute("disabled", ""))
/// }
/// ```
///
pub fn attribute(name: String, value: String) -> Selector {
  HasAttribute(name:, value:)
}

/// Select elements that include the given space-separated class name(s). For
/// example given the element `<div class="foo bar baz">`, the following selectors
/// would match:
///
/// - `query.class("foo")`
///
/// - `query.class("bar baz")`
///
/// If you need to match the class attribute exactly, you can use the [`attribute`](#attribute)
/// selector instead.
///
pub fn class(name: String) -> Selector {
  HasClass(name)
}

/// Select elements that have the specified inline style with the given value.
/// If the value is left blank, this selector will match any element that has
/// the given style, _regardless of its value_.
///
pub fn style(name: String, value: String) -> Selector {
  HasStyle(name:, value:)
}

/// Select an element based on its `id` attribute. Well-formed HTML means that
/// only one element should have a given id.
///
pub fn id(name: String) -> Selector {
  HasAttribute(name: "id", value: name)
}

/// Select elements that have the given `data-*` attribute. For example you can
/// select a custom disclosure element that is currently open with:
///
/// ```gleam
/// import lustre/dev/query
///
/// pub fn example() {
///   query.element(matching: query.data("open", "true"))
/// }
/// ```
///
pub fn data(name: String, value: String) -> Selector {
  HasAttribute(name: "data-" <> name, value: value)
}

/// It is a common convention to use the `data-test-id` attribute to mark elements
/// for easy selection in tests. This function is a shorthand for writing
/// `query.data("test-id", value)`
///
pub fn test_id(value: String) -> Selector {
  data("test-id", value)
}

/// Select elements that have the given `aria-*` attribute. For example you can
/// select the trigger of a dropdown menu with:
///
/// ```gleam
/// import lustre/dev/query
///
/// pub fn example() {
///   query.element(matching: query.aria("expanded", "true"))
/// }
/// ```
///
pub fn aria(name: String, value: String) -> Selector {
  HasAttribute(name: "aria-" <> name, value: value)
}

/// Select elements whose text content matches the given string exactly. This
/// includes text from **inline** children, but not from **block** children. For
/// example, given the following HTML:
///
/// ```html
/// <p>Hello, <span class="font-bold">Joe</span>!</p>
/// ```
///
/// The selector `query.text("Hello, Joe!")` would match the `<p>` element because
/// the text content of the inline `<span>` element is included in the paragraph's
/// text content.
///
/// Whitespace must match exactly, so the selector `query.text("Hello, Joe!")`
/// would not match an element like:
///
/// ```gleam
/// html.p([], [html.text("Hello,     Joe!")])
/// ```
///
/// > **Note**: while this selector makes a best-effort attempt to include the
/// > text content of inline children, this cannot account for block elements that
/// > are styled as inline by CSS stylesheets.
///
/// > **Note**: often it is better to use more precise selectors such as
/// > [`id`](#id), [`class`](#class), or [`test_id`](#test_id). You should reach
/// > for this selector only when you want to assert that an element contains
/// > some specific text, such as in a hero banner or a copyright notice.
///
pub fn text(content: String) -> Selector {
  Contains(content:)
}

// SEARCHING -------------------------------------------------------------------

/// Find the first element in a view that matches the given [`Query`](#Query).
/// This is useful for tests when combined with [`element.to_readable_string`](../element.html#to_readable_string),
/// allowing you to render large views but take more precise snapshots.
///
pub fn find(
  in root: Element(msg),
  matching query: Query,
) -> Result(Element(msg), Nil) {
  case find_path(in: root, matching: query, index: 0, from: path.root) {
    Ok(#(element, _)) -> Ok(element)
    Error(_) -> constants.error_nil
  }
}

///
///
@internal
pub fn find_path(
  in root: Element(msg),
  matching query: Query,
  index index: Int,
  from path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case query {
    FindElement(matching: selector) ->
      case matches(root, selector) {
        True -> Ok(#(root, path |> path.add(index, root.key)))
        False -> find_in_children(root, query, index, path)
      }

    FindChild(of: parent, matching: selector) ->
      case find_path(in: root, matching: parent, index:, from: path) {
        Ok(#(element, path)) -> find_direct_child(element, selector, path)
        Error(_) -> constants.error_nil
      }

    FindDescendant(of: parent, matching: selector) ->
      case find_path(in: root, matching: parent, index:, from: path) {
        Ok(#(element, path)) -> find_descendant(element, selector, path)
        Error(_) -> constants.error_nil
      }
  }
}

fn find_in_children(
  element: Element(msg),
  query: Query,
  index: Int,
  path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case element {
    Element(children:, ..) | Fragment(children:, ..) ->
      find_in_list(children, query, path |> path.add(index, element.key), 0)
    Map(element:, ..) -> find_in_children(element, query, index, path)
    UnsafeInnerHtml(..) -> constants.error_nil
    Text(..) -> constants.error_nil
  }
}

fn find_in_list(
  elements: List(Element(msg)),
  query: Query,
  path: Path,
  index: Int,
) -> Result(#(Element(msg), Path), Nil) {
  case elements {
    [] -> constants.error_nil

    [first, ..rest] -> {
      case find_path(in: first, matching: query, from: path, index:) {
        Ok(element) -> Ok(element)
        Error(_) -> find_in_list(rest, query, path, index + 1)
      }
    }
  }
}

fn find_direct_child(
  parent: Element(msg),
  selector: Selector,
  path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case parent {
    Element(children:, ..) | Fragment(children:, ..) ->
      find_matching_in_list(children, selector, path, 0)

    Map(element: parent, ..) -> find_direct_child(parent, selector, path)

    UnsafeInnerHtml(..) | Text(..) -> constants.error_nil
  }
}

fn find_matching_in_list(
  elements: List(Element(msg)),
  selector: Selector,
  path: Path,
  index: Int,
) -> Result(#(Element(msg), Path), Nil) {
  case elements {
    [] -> constants.error_nil

    [Fragment(..) as first, ..rest] ->
      find_matching_in_list(
        list.append(first.children, rest),
        selector,
        path.add(path, index, first.key),
        0,
      )

    [first, ..rest] ->
      case matches(first, selector) {
        True -> Ok(#(first, path.add(path, index, first.key)))
        False -> find_matching_in_list(rest, selector, path, index + 1)
      }
  }
}

fn find_descendant(
  parent: Element(msg),
  selector: Selector,
  path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case find_direct_child(parent, selector, path) {
    Ok(element) -> Ok(element)
    Error(_) ->
      case parent {
        Element(children:, ..) | Fragment(children:, ..) ->
          find_descendant_in_list(children, selector, path, 0)

        Map(element: parent, ..) -> find_descendant(parent, selector, path)

        UnsafeInnerHtml(..) | Text(..) -> constants.error_nil
      }
  }
}

fn find_descendant_in_list(
  elements: List(Element(msg)),
  selector: Selector,
  path: Path,
  index: Int,
) -> Result(#(Element(msg), Path), Nil) {
  case elements {
    [] -> constants.error_nil
    [first, ..rest] -> {
      case matches(first, selector) {
        True -> Ok(#(first, path.add(path, index, first.key)))
        False -> {
          let child = path.add(path, index, first.key)

          case find_descendant(first, selector, child) {
            Ok(element) -> Ok(element)
            Error(_) -> find_descendant_in_list(rest, selector, path, index + 1)
          }
        }
      }
    }
  }
}

/// Like [`find`](#find) but returns every element in the view that matches the
/// given query.
///
pub fn find_all(
  in root: Element(msg),
  matching query: Query,
) -> List(Element(msg)) {
  case query {
    FindElement(matching: selector) ->
      case matches(root, selector) {
        True -> [root, ..find_all_in_children(root, query)]
        False -> find_all_in_children(root, query)
      }

    FindChild(of: parent, matching: selector) ->
      root
      |> find_all(matching: parent)
      |> list.flat_map(find_all_direct_children(_, selector))

    FindDescendant(of: parent, matching: selector) ->
      root
      |> find_all(matching: parent)
      |> list.flat_map(find_all_descendants(_, selector))
  }
}

fn find_all_in_children(
  element: Element(msg),
  query: Query,
) -> List(Element(msg)) {
  case element {
    Element(children:, ..) | Fragment(children:, ..) ->
      find_all_in_list(children, query)
    Map(element:, ..) -> find_all_in_children(element, query)
    UnsafeInnerHtml(..) -> []
    Text(..) -> []
  }
}

fn find_all_in_list(
  elements: List(Element(msg)),
  query: Query,
) -> List(Element(msg)) {
  case elements {
    [] -> []
    [first, ..rest] -> {
      let first_matches = find_all(in: first, matching: query)
      let rest_matches = find_all_in_list(rest, query)

      list.append(first_matches, rest_matches)
    }
  }
}

fn find_all_direct_children(
  parent: Element(msg),
  selector: Selector,
) -> List(Element(msg)) {
  case parent {
    Element(children:, ..) | Fragment(children:, ..) ->
      find_all_matching_in_list(children, selector)
    Map(element:, ..) -> find_all_direct_children(element, selector)
    UnsafeInnerHtml(..) | Text(..) -> []
  }
}

fn find_all_matching_in_list(
  elements: List(Element(msg)),
  selector: Selector,
) -> List(Element(msg)) {
  case elements {
    [] -> []
    [first, ..rest] ->
      case matches(first, selector) {
        True -> [first, ..find_all_matching_in_list(rest, selector)]
        False -> find_all_matching_in_list(rest, selector)
      }
  }
}

fn find_all_descendants(
  parent: Element(msg),
  selector: Selector,
) -> List(Element(msg)) {
  let direct_matches = find_all_direct_children(parent, selector)
  let descendant_matches = case parent {
    Element(children:, ..) | Fragment(children:, ..) ->
      find_all_descendants_in_list(children, selector)

    Map(element: parent, ..) -> find_all_descendants(parent, selector)

    UnsafeInnerHtml(..) -> []
    Text(..) -> []
  }

  list.append(direct_matches, descendant_matches)
}

fn find_all_descendants_in_list(
  elements: List(Element(msg)),
  selector: Selector,
) -> List(Element(msg)) {
  case elements {
    [] -> []
    [first, ..rest] -> {
      let first_matches = find_all_descendants(first, selector)
      let rest_matches = find_all_descendants_in_list(rest, selector)

      list.append(first_matches, rest_matches)
    }
  }
}

/// Check if an element or any of its descendants match the given
/// [`Selector`](#Selector).
///
pub fn has(in element: Element(msg), matching selector: Selector) -> Bool {
  case find(in: element, matching: FindElement(matching: selector)) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Check if the given target element matches the given [`Selector`](#Selector).
///
pub fn matches(
  target element: Element(msg),
  selector selector: Selector,
) -> Bool {
  case element, selector {
    _, All(of: selectors) -> list.all(selectors, matches(element, _))

    Element(namespace:, tag:, ..), Type(..)
    | UnsafeInnerHtml(namespace:, tag:, ..), Type(..)
    -> {
      namespace == selector.namespace && tag == selector.tag
    }

    Element(attributes:, ..), HasAttribute(name:, value: "")
    | UnsafeInnerHtml(attributes:, ..), HasAttribute(name:, value: "")
    ->
      list.any(attributes, fn(attribute) {
        case attribute {
          Attribute(..) -> attribute.name == name
          _ -> False
        }
      })

    Element(attributes:, ..), HasAttribute(name:, value:)
    | UnsafeInnerHtml(attributes:, ..), HasAttribute(name:, value:)
    -> list.contains(attributes, attribute.attribute(name, value))

    Element(attributes:, ..), HasClass(name)
    | UnsafeInnerHtml(attributes:, ..), HasClass(name)
    -> {
      use _, class <- list.fold_until(string.split(name, " "), True)
      let name = string.trim_end(class)
      let matches =
        list.any(attributes, fn(attribute) {
          case attribute {
            Attribute(name: "class", value:, ..) ->
              value == name
              || string.starts_with(value, name <> " ")
              || string.ends_with(value, " " <> name)
              || string.contains(value, " " <> name <> " ")

            _ -> False
          }
        })

      case matches {
        True -> list.Continue(True)
        False -> list.Stop(False)
      }
    }

    Element(attributes:, ..), HasStyle(name:, value:)
    | UnsafeInnerHtml(attributes:, ..), HasStyle(name:, value:)
    -> {
      let rule = name <> ":" <> value <> ";"

      list.any(attributes, fn(attribute) {
        case attribute {
          Attribute(name: "style", value:, ..) -> string.contains(value, rule)
          _ -> False
        }
      })
    }

    Element(..), Contains(content:) -> {
      element
      |> text_content(False, "")
      |> string.contains(content)
    }

    _, _ -> False
  }
}

fn text_content(element: Element(msg), inline: Bool, content: String) -> String {
  case element {
    Fragment(..) ->
      list.fold(element.children, content, fn(content, child) {
        text_content(child, True, content)
      })

    Map(element:, ..) -> text_content(element, inline, content)

    Element(..) if !inline || element.namespace != "" ->
      list.fold(element.children, content, fn(content, child) {
        text_content(child, True, content)
      })

    Element(tag: "a", ..)
    | Element(tag: "abbr", ..)
    | Element(tag: "acronym", ..)
    | Element(tag: "b", ..)
    | Element(tag: "bdo", ..)
    | Element(tag: "big", ..)
    | Element(tag: "br", ..)
    | Element(tag: "button", ..)
    | Element(tag: "cite", ..)
    | Element(tag: "code", ..)
    | Element(tag: "dfn", ..)
    | Element(tag: "em", ..)
    | Element(tag: "i", ..)
    | Element(tag: "img", ..)
    | Element(tag: "input", ..)
    | Element(tag: "kbd", ..)
    | Element(tag: "label", ..)
    | Element(tag: "map", ..)
    | Element(tag: "object", ..)
    | Element(tag: "output", ..)
    | Element(tag: "q", ..)
    | Element(tag: "samp", ..)
    | Element(tag: "script", ..)
    | Element(tag: "select", ..)
    | Element(tag: "small", ..)
    | Element(tag: "span", ..)
    | Element(tag: "strong", ..)
    | Element(tag: "sub", ..)
    | Element(tag: "sup", ..)
    | Element(tag: "textarea", ..)
    | Element(tag: "time", ..)
    | Element(tag: "tt", ..)
    | Element(tag: "var", ..) ->
      list.fold(element.children, content, fn(content, child) {
        text_content(child, True, content)
      })

    Element(..) -> {
      // This doesn't include the semi-colon at the end of the rule because we
      // want to match any of the following:
      //
      //   display:inline
      //   display:inline-block
      //   display:inline grid
      //
      let rule = "display:inline"
      let is_inline =
        list.any(element.attributes, fn(attribute) {
          case attribute {
            Attribute(name: "style", value:, ..) -> string.contains(value, rule)
            _ -> False
          }
        })

      case is_inline {
        True ->
          list.fold(element.children, content, fn(content, child) {
            text_content(child, True, content)
          })

        False -> content
      }
    }

    Text(..) -> content <> element.content

    UnsafeInnerHtml(..) -> content
  }
}

// PRETTY PRINTING -------------------------------------------------------------

/// Print a `Query` as a human-readable string similar to a CSS selector. This
/// function is primarily intended for debugging and testing purposes: for example,
/// you might use this to include the selector in a snapshot test for easier
/// review.
///
/// > **Note**: while similar, this function is not guaranteed to produce a valid
/// > CSS selector. Specifically, queries that use the [`text`](#text) selector
/// > will not be valid CSS selectors as they use the `:contains` pseudo-class,
/// > which is not part of the CSS spec!
///
pub fn to_readable_string(query: Query) -> String {
  case query {
    FindElement(matching: selector) -> selector_to_readable_string(selector)

    FindChild(of: parent, matching: selector) ->
      to_readable_string(parent)
      <> " > "
      <> selector_to_readable_string(selector)

    FindDescendant(of: parent, matching: selector) ->
      to_readable_string(parent) <> " " <> selector_to_readable_string(selector)
  }
}

fn selector_to_readable_string(selector: Selector) -> String {
  case selector {
    All(of: [])
    | Type(namespace: "", tag: "")
    | HasAttribute(name: "", ..)
    | HasClass(name: "")
    | HasStyle(name: "", ..)
    | HasStyle(value: "", ..)
    | Contains(content: "") -> ""

    All(of: selectors) ->
      selectors
      |> sort_selectors
      |> list.map(selector_to_readable_string)
      |> string.concat

    Type(namespace: "", tag:) -> tag
    Type(namespace:, tag:) -> namespace <> ":" <> tag
    HasAttribute(name: "id", value:) -> "#" <> value
    HasAttribute(name:, value: "") -> "[" <> name <> "]"
    HasAttribute(name:, value:) -> "[" <> name <> "=\"" <> value <> "\"]"
    HasClass(name:) -> "." <> name
    HasStyle(name:, value:) -> "[style*=\"" <> name <> ":" <> value <> "\"]"
    Contains(content:) -> ":contains(\"" <> content <> "\")"
  }
}

fn sort_selectors(selectors: List(Selector)) -> List(Selector) {
  use a, b <- list.sort({
    use selector <- list.flat_map(selectors)

    case selector {
      All(of: selectors) -> selectors
      _ -> [selector]
    }
  })

  case a, b {
    All(..), _ | _, All(..) -> panic as "`All` selectors should be flattened"

    Type(..), Type(..) ->
      case string.compare(a.namespace, b.namespace) {
        order.Eq -> string.compare(a.tag, b.tag)
        order -> order
      }

    Type(..), _ -> order.Lt
    _, Type(..) -> order.Gt

    HasAttribute(name: "id", ..), HasAttribute(name: "id", ..) ->
      string.compare(a.value, b.value)

    HasAttribute(name: "id", ..), _ -> order.Lt
    _, HasAttribute(name: "id", ..) -> order.Gt

    HasAttribute(..), HasAttribute(..) ->
      case string.compare(a.name, b.name) {
        order.Eq -> string.compare(a.value, b.value)
        order -> order
      }

    HasAttribute(..), _ -> order.Lt
    _, HasAttribute(..) -> order.Gt

    HasStyle(..), HasStyle(..) -> string.compare(a.name, b.name)
    HasStyle(..), _ -> order.Lt
    _, HasStyle(..) -> order.Gt

    HasClass(..), HasClass(..) -> string.compare(a.name, b.name)
    HasClass(..), _ -> order.Lt
    _, HasClass(..) -> order.Gt

    Contains(..), Contains(..) -> string.compare(a.content, b.content)
  }
}
