//// This module contains the DOM platform implementation and HTML-specific
//// rendering utilities. The DOM platform provides the low-level mutation methods
//// needed to render Lustre applications to the browser DOM.
////
//// For HTML serialization and SSR, see the [`SerializerConfig`](#SerializerConfig) type and
//// related functions like [`to_string`](#to_string) and [`serialize`](#serialize).
////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam/string_tree.{type StringTree}
import houdini
import lustre/platform.{type Platform, type PlatformError}
import lustre/serializer.{type Serializer, Serializer}
import lustre/vdom/vattr
import lustre/vdom/vnode.{
  type Element, Element, Fragment, Map, Memo, RawContainer, Text,
}

// TYPES -----------------------------------------------------------------------

/// A type representing a DOM node in JavaScript.
///
pub type DomNode

/// A type representing a DOM event in JavaScript.
///
pub type DomEvent

/// Configuration for HTML serialization. Controls which elements are treated as
/// void elements (no closing tag, like `<br>`) or self-closing tags (XML style,
/// like `<path />`).
///
/// Use [`serializer_config`](#serializer_config) to create a config with default HTML void
/// elements, or [`empty_serializer_config`](#empty_serializer_config) to start from scratch.
/// Convert to a [`Serializer`](../platform.html#Serializer) using
/// [`to_serializer`](#to_serializer).
///
pub opaque type SerializerConfig {
  SerializerConfig(void_elements: Set(String), self_closing_tags: Set(String))
}

// DOM PLATFORM CONSTRUCTORS ---------------------------------------------------

/// Returns a [`Platform`](../platform.html#Platform) configured for the browser
/// DOM. This is the standard platform used by client-side Lustre applications.
///
/// The `target` argument is a CSS selector used to locate the DOM element where
/// the application will be mounted. The selector is resolved at construction
/// time, and the resolved DOM node is stored as the platform's target.
///
/// On Erlang this always returns `Error(NotABrowser)`. On JavaScript this
/// succeeds when running in a browser environment and the selector matches an
/// element.
///
pub fn platform(
  onto target: String,
) -> Result(Platform(DomNode, DomNode, DomNode, DomEvent, msg), PlatformError) {
  use <- bool.guard(!platform.is_browser(), Error(platform.NotABrowser))
  use target <- result.try(do_query_selector(target))

  Ok(platform_strict(target))
}

/// Returns a [`Platform`](../platform.html#Platform) configured for the browser
/// DOM, using a known-good DOM node as the root. Unlike [`dom`](#dom), this
/// function does not perform a selector query and cannot fail.
///
/// This is used internally for web components (where the shadow root is already
/// available) and can be used by advanced users who already have a reference to
/// a DOM node.
///
@external(javascript, "./dom.ffi.mjs", "dom_strict")
@internal
pub fn platform_strict(
  onto _root: DomNode,
) -> Platform(DomNode, DomNode, DomNode, DomEvent, msg) {
  panic as "Cannot create DOM platform on Erlang"
}

fn do_query_selector(selector: String) -> Result(DomNode, PlatformError) {
  case do_query_selector_raw(selector) {
    Ok(node) -> Ok(node)
    Error(sel) -> Error(platform.ElementNotFound(sel))
  }
}

@external(javascript, "./dom.ffi.mjs", "query_selector")
fn do_query_selector_raw(_selector: String) -> Result(DomNode, String) {
  Error("")
}

// SERIALIZER ------------------------------------------------------------------

/// Returns a [`Serializer`](../platform.html#Serializer) that converts elements
/// to HTML strings using the default HTML void elements. This is the standard
/// serializer for server-side rendering.
///
/// ## Example
///
/// ```gleam
/// import lustre/platform
/// import lustre/platform/dom
///
/// let p = platform.headless(dom.serializer())
/// ```
///
pub fn serializer() -> Serializer(msg) {
  Serializer(element: to_string, raw_content: vnode.raw_content_to_string)
}

/// Converts an [`SerializerConfig`](#SerializerConfig) to a
/// [`Serializer`](../platform.html#Serializer) function.
///
/// ## Example
///
/// ```gleam
/// import lustre/platform
/// import lustre/platform/dom
///
/// let s = dom.serializer_config()
///   |> dom.with_void("custom-void")
///   |> dom.to_serializer()
///
/// let p = platform.headless(s)
/// ```
///
pub fn to_serializer(config: SerializerConfig) -> Serializer(msg) {
  Serializer(
    element: fn(element) { serialize(config, element) },
    raw_content: vnode.raw_content_to_string,
  )
}

// SERIALIZER CONFIG -----------------------------------------------------------

/// The default HTML void elements that cannot have children and should not
/// have a closing tag.
const default_void_elements: List(String) = [
  "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta",
  "param", "source", "track", "wbr",
]

/// Create an HTML config with default void elements and no self-closing tags.
/// This is the standard configuration for rendering HTML.
///
/// ## Example
///
/// ```gleam
/// let config = dom.serializer_config()
///   |> dom.with_void("custom-void")
/// ```
///
pub fn serializer_config() -> SerializerConfig {
  SerializerConfig(
    void_elements: set.from_list(default_void_elements),
    self_closing_tags: set.new(),
  )
}

/// Create an empty HTML config with no void elements and no self-closing tags.
/// Use this as a starting point for custom configurations.
///
/// ## Example
///
/// ```gleam
/// let config = dom.empty_serializer_config()
///   |> dom.with_self_closing_tags(["path", "circle", "rect"])
/// ```
///
pub fn empty_serializer_config() -> SerializerConfig {
  SerializerConfig(void_elements: set.new(), self_closing_tags: set.new())
}

/// Add a void element to the config. Void elements render without a closing
/// tag (e.g., `<br>` instead of `<br></br>`).
///
pub fn with_void(config: SerializerConfig, tag: String) -> SerializerConfig {
  SerializerConfig(
    ..config,
    void_elements: set.insert(config.void_elements, tag),
  )
}

/// Add multiple void elements to the config.
///
pub fn with_void_elements(
  config: SerializerConfig,
  tags: List(String),
) -> SerializerConfig {
  SerializerConfig(
    ..config,
    void_elements: list.fold(tags, config.void_elements, fn(acc, tag) {
      set.insert(acc, tag)
    }),
  )
}

/// Add a self-closing tag to the config. Self-closing tags render with
/// XML-style self-closing syntax (e.g., `<path />` instead of `<path></path>`).
///
pub fn with_self_closing(
  config: SerializerConfig,
  tag: String,
) -> SerializerConfig {
  SerializerConfig(
    ..config,
    self_closing_tags: set.insert(config.self_closing_tags, tag),
  )
}

/// Add multiple self-closing tags to the config.
///
pub fn with_self_closing_tags(
  config: SerializerConfig,
  tags: List(String),
) -> SerializerConfig {
  SerializerConfig(
    ..config,
    self_closing_tags: list.fold(tags, config.self_closing_tags, fn(acc, tag) {
      set.insert(acc, tag)
    }),
  )
}

/// Check if a tag is a void element in this config.
/// Only applies to elements in the default HTML namespace (empty string).
///
pub fn is_void(config: SerializerConfig, tag: String, namespace: String) -> Bool {
  case namespace {
    "" -> set.contains(config.void_elements, tag)
    _ -> False
  }
}

/// Check if a tag is a self-closing tag in this config.
///
pub fn is_self_closing(config: SerializerConfig, tag: String) -> Bool {
  set.contains(config.self_closing_tags, tag)
}

// HTML VOID ELEMENTS ----------------------------------------------------------

/// Check if a tag is an HTML void element (cannot have children and should not
/// have a closing tag). Only applies to elements in the default HTML namespace.
///
/// This uses the default HTML void elements list. For custom configurations,
/// use a [`SerializerConfig`](#SerializerConfig) instead.
///
fn is_void_element(tag: String, namespace: String) -> Bool {
  case namespace {
    "" ->
      case tag {
        "area"
        | "base"
        | "br"
        | "col"
        | "embed"
        | "hr"
        | "img"
        | "input"
        | "link"
        | "meta"
        | "param"
        | "source"
        | "track"
        | "wbr" -> True
        _ -> False
      }
    _ -> False
  }
}

// CONFIG-BASED RENDERING ------------------------------------------------------

/// Convert a Lustre `Element` to an HTML string using the given HTML config.
/// This is _not_ pretty-printed, so there are no newlines or indentation.
///
/// ## Example
///
/// ```gleam
/// let config = dom.serializer_config()
///   |> dom.with_void("custom-void")
/// dom.serialize(config, element("custom-void", [], []))
/// // -> "<custom-void>"
/// ```
///
pub fn serialize(config: SerializerConfig, node: Element(msg)) -> String {
  node
  |> serialize_tree(config, _, "")
  |> string_tree.to_string
}

/// Convert a Lustre `Element` to an HTML `StringTree` using the given HTML config.
/// This is more efficient than `serialize` when the output will be written to a
/// socket or file.
///
pub fn serialize_tree(
  config: SerializerConfig,
  node: Element(msg),
  parent_namespace: String,
) -> StringTree {
  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) -> string_tree.from_string(houdini.escape(content))

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        attrs_to_string_tree(key, namespace, parent_namespace, attributes)

      // Check self-closing first, then void
      case is_self_closing(config, tag), is_void(config, tag, namespace) {
        True, _ ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append("/>")
        _, True ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">")
        False, False ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">")
          |> config_children_to_string_tree(config, children, namespace)
          |> string_tree.append("</" <> tag <> ">")
      }
    }

    RawContainer(key:, namespace:, tag:, attributes:, content:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        attrs_to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append(vnode.raw_content_to_string(content))
      |> string_tree.append("</" <> tag <> ">")
    }

    Fragment(key:, children:, ..) -> {
      marker_comment("lustre:fragment", key)
      |> config_children_to_string_tree(config, children, parent_namespace)
      |> string_tree.append_tree(marker_comment("/lustre:fragment", ""))
    }

    Map(key:, child:, ..) -> {
      marker_comment("lustre:map", key)
      |> string_tree.append_tree(serialize_tree(config, child, parent_namespace))
    }

    Memo(key:, view:, ..) -> {
      marker_comment("lustre:memo", key)
      |> string_tree.append_tree(serialize_tree(
        config,
        view(),
        parent_namespace,
      ))
    }
  }
}

fn config_children_to_string_tree(
  html: StringTree,
  config: SerializerConfig,
  children: List(Element(msg)),
  namespace: String,
) -> StringTree {
  use html, child <- list.fold(children, html)
  string_tree.append_tree(html, serialize_tree(config, child, namespace))
}

// STRING RENDERING ------------------------------------------------------------

/// Convert a Lustre `Element` to an HTML string. This is _not_ pretty-printed,
/// so there are no newlines or indentation.
///
/// This uses the default HTML void elements. For custom void or self-closing
/// configurations, use [`serialize`](#serialize) with a custom
/// [`SerializerConfig`](#SerializerConfig).
///
pub fn to_string(node: Element(msg)) -> String {
  node
  |> to_string_tree("")
  |> string_tree.to_string
}

/// Convert a Lustre `Element` to an HTML `StringTree`. This is more efficient
/// than `to_string` when the output will be written to a socket or file.
///
/// This uses the default HTML void elements list. For custom void or
/// self-closing configurations, use [`serialize_tree`](#serialize_tree) with
/// a custom [`SerializerConfig`](#SerializerConfig).
///
pub fn to_string_tree(
  node: Element(msg),
  parent_namespace: String,
) -> StringTree {
  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) -> string_tree.from_string(houdini.escape(content))

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        attrs_to_string_tree(key, namespace, parent_namespace, attributes)

      // Compute void-ness at render time using the default HTML void elements.
      case is_void_element(tag, namespace) {
        True ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">")
        False ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">")
          |> children_to_string_tree(children, namespace)
          |> string_tree.append("</" <> tag <> ">")
      }
    }

    RawContainer(key:, namespace:, tag:, attributes:, content:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        attrs_to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append(vnode.raw_content_to_string(content))
      |> string_tree.append("</" <> tag <> ">")
    }

    Fragment(key:, children:, ..) -> {
      marker_comment("lustre:fragment", key)
      |> children_to_string_tree(children, parent_namespace)
      |> string_tree.append_tree(marker_comment("/lustre:fragment", ""))
    }

    Map(key:, child:, ..) -> {
      marker_comment("lustre:map", key)
      |> string_tree.append_tree(to_string_tree(child, parent_namespace))
    }

    Memo(key:, view:, ..) -> {
      marker_comment("lustre:memo", key)
      |> string_tree.append_tree(to_string_tree(view(), parent_namespace))
    }
  }
}

fn children_to_string_tree(
  html: StringTree,
  children: List(Element(msg)),
  namespace: String,
) -> StringTree {
  use html, child <- list.fold(children, html)
  string_tree.append_tree(html, to_string_tree(child, namespace))
}

/// Converts an element to a string like [`to_string`](#to_string), but prepends
/// a `<!doctype html>` declaration to the string. This is useful for rendering
/// complete HTML documents.
///
pub fn to_document_string(el: Element(msg)) -> String {
  to_string(case el {
    Element(tag: "html", ..) -> el
    Element(tag: "head", ..) | Element(tag: "body", ..) ->
      vnode.element(
        key: "",
        namespace: "",
        tag: "html",
        attributes: [],
        children: [el],
        keyed_children: vnode.empty_keyed_children(),
      )
    _ ->
      vnode.element(
        key: "",
        namespace: "",
        tag: "html",
        attributes: [],
        children: [
          vnode.element(
            key: "",
            namespace: "",
            tag: "body",
            attributes: [],
            children: [el],
            keyed_children: vnode.empty_keyed_children(),
          ),
        ],
        keyed_children: vnode.empty_keyed_children(),
      )
  })
  |> string.append("<!doctype html>\n", _)
}

/// Converts a Lustre `Element` to an HTML `StringTree`, prepending a
/// `<!doctype html>` declaration. This is useful for rendering complete
/// HTML documents efficiently.
///
pub fn to_document_string_tree(el: Element(msg)) -> StringTree {
  to_string_tree(
    case el {
      Element(tag: "html", ..) -> el
      Element(tag: "head", ..) | Element(tag: "body", ..) ->
        vnode.element(
          key: "",
          namespace: "",
          tag: "html",
          attributes: [],
          children: [el],
          keyed_children: vnode.empty_keyed_children(),
        )
      _ ->
        vnode.element(
          key: "",
          namespace: "",
          tag: "html",
          attributes: [],
          children: [
            vnode.element(
              key: "",
              namespace: "",
              tag: "body",
              attributes: [],
              children: [el],
              keyed_children: vnode.empty_keyed_children(),
            ),
          ],
          keyed_children: vnode.empty_keyed_children(),
        )
    },
    "",
  )
  |> string_tree.prepend("<!doctype html>\n")
}

// SNAPSHOT RENDERING ----------------------------------------------------------

/// Converts a Lustre `Element` to a human-readable string by inserting new lines
/// and indentation where appropriate. This is useful for debugging and testing.
///
pub fn to_readable_string(el: Element(msg)) -> String {
  to_snapshot(el, False)
}

/// Convert an element to a snapshot string for testing. When `debug` is `True`,
/// fragment and map markers are included in the output.
///
pub fn to_snapshot(node: Element(msg), debug: Bool) -> String {
  do_to_snapshot_builder(node:, raw: False, debug:, namespace: "", indent: 0)
  |> string_tree.to_string
}

fn do_to_snapshot_builder(
  node node: Element(msg),
  raw raw: Bool,
  debug debug: Bool,
  namespace parent_namespace: String,
  indent indent: Int,
) -> StringTree {
  let spaces = string.repeat("  ", indent)

  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) if raw -> string_tree.from_strings([spaces, content])
    Text(content:, ..) ->
      string_tree.from_strings([spaces, houdini.escape(content)])

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        attrs_to_string_tree(key, namespace, parent_namespace, attributes)

      // Compute void-ness at render time using the default HTML void elements.
      case is_void_element(tag, namespace), children {
        // Void elements (like <input>, <br>) have no closing tag
        True, _ ->
          html
          |> string_tree.prepend(spaces)
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">")
        // Empty non-void elements get compact rendering
        False, [] ->
          html
          |> string_tree.prepend(spaces)
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">")
          |> string_tree.append("</" <> tag <> ">")
        // Non-void elements with children get pretty-printed
        False, _ ->
          html
          |> string_tree.prepend(spaces)
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">\n")
          |> children_to_snapshot_builder(
            children:,
            raw:,
            debug:,
            namespace:,
            indent: indent + 1,
          )
          |> string_tree.append(spaces)
          |> string_tree.append("</" <> tag <> ">")
      }
    }

    RawContainer(key:, namespace:, tag:, attributes:, content:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        attrs_to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append(vnode.raw_content_to_string(content))
      |> string_tree.append("</" <> tag <> ">")
    }

    Fragment(key:, children:, ..) if debug -> {
      marker_comment("lustre:fragment", key)
      |> string_tree.prepend(spaces)
      |> string_tree.append("\n")
      |> children_to_snapshot_builder(
        children:,
        raw:,
        debug:,
        namespace: parent_namespace,
        indent: indent + 1,
      )
      |> string_tree.append(spaces)
      |> string_tree.append_tree(marker_comment("/lustre:fragment", ""))
    }

    Fragment(children:, ..) ->
      children_to_snapshot_builder(
        html: string_tree.new(),
        children: children,
        raw: raw,
        debug: debug,
        namespace: parent_namespace,
        indent: indent,
      )

    Map(key:, child:, ..) if debug -> {
      marker_comment("lustre:map", key)
      |> string_tree.prepend(spaces)
      |> string_tree.append("\n")
      |> string_tree.append_tree(do_to_snapshot_builder(
        node: child,
        raw:,
        debug:,
        namespace: parent_namespace,
        indent: indent + 1,
      ))
    }

    Map(child:, ..) ->
      do_to_snapshot_builder(
        node: child,
        raw:,
        debug:,
        namespace: parent_namespace,
        indent:,
      )

    Memo(key:, view:, ..) if debug -> {
      marker_comment("lustre:memo", key)
      |> string_tree.prepend(spaces)
      |> string_tree.append("\n")
      |> string_tree.append_tree(do_to_snapshot_builder(
        node: view(),
        raw:,
        debug:,
        namespace: parent_namespace,
        indent: indent + 1,
      ))
    }

    Memo(view:, ..) ->
      do_to_snapshot_builder(
        node: view(),
        raw:,
        debug:,
        namespace: parent_namespace,
        indent:,
      )
  }
}

fn children_to_snapshot_builder(
  html html: StringTree,
  children children: List(Element(msg)),
  raw raw: Bool,
  debug debug: Bool,
  namespace namespace: String,
  indent indent: Int,
) -> StringTree {
  case children {
    [Text(content: a, ..), Text(content: b, ..), ..rest] ->
      children_to_snapshot_builder(
        html:,
        children: [
          Text(kind: vnode.text_kind, key: "", content: a <> b),
          ..rest
        ],
        raw:,
        debug:,
        namespace:,
        indent:,
      )

    [child, ..rest] ->
      child
      |> do_to_snapshot_builder(raw:, debug:, namespace:, indent:)
      |> string_tree.append("\n")
      |> string_tree.prepend_tree(html)
      |> children_to_snapshot_builder(
        children: rest,
        raw:,
        debug:,
        namespace:,
        indent:,
      )

    [] -> html
  }
}

fn marker_comment(label: String, key: String) {
  case key {
    "" -> string_tree.from_string("<!-- " <> label <> " -->")
    _ ->
      string_tree.from_string("<!-- " <> label <> " key=\"")
      |> string_tree.append(houdini.escape(key))
      |> string_tree.append("\" -->")
  }
}

// STRING RENDERING ------------------------------------------------------------

pub fn attrs_to_string_tree(
  key: String,
  namespace: String,
  parent_namespace: String,
  attributes: List(vattr.Attribute(msg)),
) -> StringTree {
  let attributes = case key != "" {
    True -> [vattr.attribute("data-lustre-key", key), ..attributes]
    False -> attributes
  }

  let attributes = case namespace != parent_namespace {
    True if namespace == "" -> [
      vattr.attribute("xmlns", "http://www.w3.org/1999/xhtml"),
      ..attributes
    ]

    True -> [vattr.attribute("xmlns", namespace), ..attributes]

    False -> attributes
  }

  use html, attr <- list.fold(attributes, string_tree.new())

  case attr {
    // We special-case this "virtual" attribute to stringify as a regular `"value"`
    // attribute. In HTML, the default value of an input is set by this value
    // attribute, but in Lustre users would use the `attribute.value` function
    // for inputs that should be controlled by their model.
    vattr.Attribute(name: "virtual:defaultValue", value:, ..) ->
      string_tree.append(html, " value=\"" <> houdini.escape(value) <> "\"")

    vattr.Attribute(name: "virtual:defaultChecked", ..) ->
      string_tree.append(html, " checked")

    vattr.Attribute(name: "virtual:defaultSelected", ..) ->
      string_tree.append(html, " selected")

    vattr.Attribute(name: "", ..) -> html
    vattr.Attribute(name:, value: "", ..) ->
      string_tree.append(html, " " <> name)
    vattr.Attribute(name:, value:, ..) ->
      string_tree.append(html, {
        " " <> name <> "=\"" <> houdini.escape(value) <> "\""
      })
    _ -> html
  }
}
