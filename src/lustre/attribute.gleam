// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import lustre/internals/constants
import lustre/vdom/attribute.{Attribute, Event, Property}

// TYPES -----------------------------------------------------------------------

/// The `Attribute` type encompasses HTML attributes, DOM properties, and
/// event listeners.
///
pub type Attribute(msg) =
  attribute.Attribute(msg)

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an HTML attribute. This is like saying `element.setAttribute("class", "wibble")`
/// in JavaScript. Attributes will be rendered when calling [`element.to_string`](./element.html#to_string).
///
/// **Note**: there is a subtle difference between attributes and properties. You
/// can read more about the implications of this
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn attribute(name: String, value: String) -> Attribute(msg) {
  Attribute(name, value)
}

/// Create a DOM property. This is like saying `element.className = "wibble"` in
/// JavaScript. Properties will be **not** be rendered when calling
/// [`element.to_string`](./element.html#to_string).
///
/// **Note**: there is a subtle difference between attributes and properties. You
/// can read more about the implications of this
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn property(name: String, value: Json) -> Attribute(msg) {
  Property(name, value)
}

///
pub fn on(name: String, handler: Decoder(msg)) -> Attribute(msg) {
  Event(
    name:,
    handler:,
    include: constants.empty_list,
    prevent_default: False,
    stop_propagation: False,
    immediate: False,
  )
}

/// Create an empty attribute. This is not added to the DOM and not rendered when
/// calling [`element.to_string`](./element.html#to_string), but it is useful for
/// _conditionally_ adding attributes to an element.
///
pub fn none() -> Attribute(msg) {
  class("")
}

// MANIPULATIONS ---------------------------------------------------------------

/// The `Attribute` type is parameterised by the type of messages it can produce
/// from events handlers. Sometimes you might end up with an attribute from a
/// library or module that produces a different type of message: this function lets
/// you map the messages produced from one type to another.
///
pub fn map(attribute: Attribute(a), f: fn(a) -> b) -> Attribute(b) {
  case attribute {
    Attribute(name, value) -> Attribute(name, value)
    Event(handler:, ..) -> Event(..attribute, handler: decode.map(handler, f))
    Property(name, value) -> Property(name, value)
  }
}

// COMMON ATTRIBUTES -----------------------------------------------------------

///
///
/// > **Note**: unlike most attributes, multiple `style` attributes are merged
/// > with any existing other styles on an element. Styles added _later_ in the
/// > list will override styles added earlier.
///
pub fn style(properties: List(#(String, String))) -> Attribute(msg) {
  attribute("style", {
    use styles, #(name, value) <- list.fold(properties, "")
    styles <> name <> ":" <> value <> ";"
  })
}

///
///
/// > **Note**: unlike most attributes, multiple `class` attributes are merged
/// > with any existing other classes on an element.
///
pub fn class(name: String) -> Attribute(msg) {
  attribute("class", name)
}

///
pub fn classes(names: List(#(String, Bool))) -> Attribute(msg) {
  attribute(
    "class",
    names
      |> list.filter_map(fn(class) {
        case class.1 {
          True -> Ok(class.0)
          False -> constants.error_nil
        }
      })
      |> string.join(" "),
  )
}

///
pub fn id(name: String) -> Attribute(msg) {
  attribute("id", name)
}

///
pub fn role(name: String) -> Attribute(msg) {
  attribute("role", name)
}

///
pub fn title(name: String) -> Attribute(msg) {
  attribute("title", name)
}

// INPUTS ----------------------------------------------------------------------

///
pub fn type_(name: String) -> Attribute(msg) {
  attribute("type", name)
}

///
pub fn value(val: String) -> Attribute(msg) {
  attribute("value", val)
}

///
pub fn checked(is_checked: Bool) -> Attribute(msg) {
  property("checked", json.bool(is_checked))
}

///
pub fn placeholder(text: String) -> Attribute(msg) {
  attribute("placeholder", text)
}

///
pub fn selected(is_selected: Bool) -> Attribute(msg) {
  property("selected", json.bool(is_selected))
}

// INPUT HELPERS ---------------------------------------------------------------

///
pub fn accept(types: List(String)) -> Attribute(msg) {
  attribute("accept", string.join(types, ","))
}

///
pub fn accept_charset(types: List(String)) -> Attribute(msg) {
  attribute("acceptCharset", string.join(types, " "))
}

///
pub fn msg(uri: String) -> Attribute(msg) {
  attribute("msg", uri)
}

///
pub fn autocomplete(name: String) -> Attribute(msg) {
  attribute("autocomplete", name)
}

///
pub fn autofocus(should_autofocus: Bool) -> Attribute(msg) {
  property("autofocus", json.bool(should_autofocus))
}

///
pub fn disabled(is_disabled: Bool) -> Attribute(msg) {
  property("disabled", json.bool(is_disabled))
}

///
pub fn name(name: String) -> Attribute(msg) {
  attribute("name", name)
}

///
pub fn pattern(regex: String) -> Attribute(msg) {
  attribute("pattern", regex)
}

///
pub fn readonly(is_readonly: Bool) -> Attribute(msg) {
  property("readOnly", json.bool(is_readonly))
}

///
pub fn required(is_required: Bool) -> Attribute(msg) {
  property("required", json.bool(is_required))
}

///
pub fn for(id: String) -> Attribute(msg) {
  attribute("for", id)
}

// INPUT RANGES ----------------------------------------------------------------

///
pub fn max(val: String) -> Attribute(msg) {
  attribute("max", val)
}

///
pub fn min(val: String) -> Attribute(msg) {
  attribute("min", val)
}

///
pub fn step(val: String) -> Attribute(msg) {
  attribute("step", val)
}

// INPUT TEXT AREAS ------------------------------------------------------------

///
pub fn cols(val: Int) -> Attribute(msg) {
  attribute("cols", int.to_string(val))
}

///
pub fn rows(val: Int) -> Attribute(msg) {
  attribute("rows", int.to_string(val))
}

///
pub fn wrap(mode: String) -> Attribute(msg) {
  attribute("wrap", mode)
}

// LINKS AND AREAS -------------------------------------------------------------

///
pub fn href(uri: String) -> Attribute(msg) {
  attribute("href", uri)
}

///
pub fn target(target: String) -> Attribute(msg) {
  attribute("target", target)
}

///
pub fn download(filename: String) -> Attribute(msg) {
  attribute("download", filename)
}

///
pub fn rel(relationship: String) -> Attribute(msg) {
  attribute("rel", relationship)
}

// EMBEDDED CONTENT ------------------------------------------------------------

///
pub fn src(uri: String) -> Attribute(msg) {
  attribute("src", uri)
}

/// **Note**: this uses [`property`](#property) to set the value directly on the
/// DOM node, making it **incompatible** with SVG elements. To set the height of
/// an `<svg>` element, use the [`attribute`](#attribute) function directly.
///
/// You can read more about the difference between attributes and properties
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn height(val: Int) -> Attribute(msg) {
  property("height", json.int(val))
}

/// **Note**: this uses [`property`](#property) to set the value directly on the
/// DOM node, making it **incompatible** with SVG elements. To set the width of
/// an `<svg>` element, use the [`attribute`](#attribute) function directly.
///
/// You can read more about the difference between attributes and properties
/// [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
///
pub fn width(val: Int) -> Attribute(msg) {
  property("width", json.int(val))
}

///
pub fn alt(text: String) -> Attribute(msg) {
  attribute("alt", text)
}

///
pub fn content(text: String) -> Attribute(msg) {
  attribute("content", text)
}

// AUDIO AND VIDEO -------------------------------------------------------------

///
pub fn autoplay(should_autoplay: Bool) -> Attribute(msg) {
  property("autoplay", json.bool(should_autoplay))
}

///
pub fn controls(visible: Bool) -> Attribute(msg) {
  property("controls", json.bool(visible))
}

///
pub fn loop(should_loop: Bool) -> Attribute(msg) {
  property("loop", json.bool(should_loop))
}

// FORMS -----------------------------------------------------------------------

///
pub fn action(url: String) -> Attribute(msg) {
  attribute("action", url)
}

///
pub fn enctype(value: String) -> Attribute(msg) {
  attribute("enctype", value)
}

///
pub fn method(method: String) -> Attribute(msg) {
  attribute("method", method)
}

///
pub fn novalidate(value: Bool) -> Attribute(msg) {
  property("novalidate", json.bool(value))
}

///
pub fn form_action(action: String) -> Attribute(msg) {
  attribute("formaction", action)
}

///
pub fn form_enctype(value: String) -> Attribute(msg) {
  attribute("formenctype", value)
}

///
pub fn form_method(method: String) -> Attribute(msg) {
  attribute("formmethod", method)
}

///
pub fn form_novalidate(value: Bool) -> Attribute(msg) {
  property("formnovalidate", json.bool(value))
}

///
pub fn form_target(target: String) -> Attribute(msg) {
  attribute("formtarget", target)
}

// DIALOGS ---------------------------------------------------------------------

///
pub fn open(is_open: Bool) -> Attribute(msg) {
  property("open", json.bool(is_open))
}

// META ------------------------------------------------------------------------

///
pub fn charset(name: String) -> Attribute(msg) {
  attribute("charset", name)
}

///
pub fn http_equiv(name: String) -> Attribute(msg) {
  attribute("http-equiv", name)
}
