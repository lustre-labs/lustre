//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/attribute](https://lustre.build/api/lustre/attribute)

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/function
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lustre/internals/vdom.{Attribute, Event}

// TYPES -----------------------------------------------------------------------

///
pub type Attribute(msg) =
  vdom.Attribute(msg)

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn attribute(name: String, value: String) -> Attribute(msg) {
  Attribute(name, dynamic.from(value), as_property: False)
}

/// 
pub fn property(name: String, value: any) -> Attribute(msg) {
  Attribute(name, dynamic.from(value), as_property: True)
}

///
pub fn on(
  name: String,
  handler: fn(Dynamic) -> Result(msg, error),
) -> Attribute(msg) {
  Event("on" <> name, function.compose(handler, result.replace_error(_, Nil)))
}

///
pub fn none() -> Attribute(msg) {
  Attribute("", dynamic.from(""), as_property: False)
}

// MANIPULATIONS ---------------------------------------------------------------

/// 
pub fn map(attr: Attribute(a), f: fn(a) -> b) -> Attribute(b) {
  case attr {
    Attribute(name, value, as_property) -> Attribute(name, value, as_property)
    Event(on, handler) -> Event(on, fn(e) { result.map(handler(e), f) })
  }
}

// COMMON ATTRIBUTES -----------------------------------------------------------

///
pub fn style(properties: List(#(String, String))) -> Attribute(msg) {
  attribute("style", {
    use styles, #(name, value) <- list.fold(properties, "")
    styles <> name <> ":" <> value <> ";"
  })
}

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
        False -> Error(Nil)
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

// INPUTS ----------------------------------------------------------------------

///
pub fn type_(name: String) -> Attribute(msg) {
  attribute("type", name)
}

///
pub fn value(val: any) -> Attribute(msg) {
  property("value", dynamic.from(val))
}

///
pub fn checked(is_checked: Bool) -> Attribute(msg) {
  property("checked", is_checked)
}

///
pub fn placeholder(text: String) -> Attribute(msg) {
  attribute("placeholder", text)
}

///
pub fn selected(is_selected: Bool) -> Attribute(msg) {
  property("selected", is_selected)
}

// INPUT HELPERS ---------------------------------------------------------------

///
pub fn accept(types: List(String)) -> Attribute(msg) {
  attribute("accept", string.join(types, " "))
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
  property("autofocus", should_autofocus)
}

///
pub fn disabled(is_disabled: Bool) -> Attribute(msg) {
  property("disabled", is_disabled)
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
  property("readonly", is_readonly)
}

///
pub fn required(is_required: Bool) -> Attribute(msg) {
  property("required", is_required)
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

///
pub fn height(val: Int) -> Attribute(msg) {
  property("height", int.to_string(val))
}

///
pub fn width(val: Int) -> Attribute(msg) {
  property("width", int.to_string(val))
}

///
pub fn alt(text: String) -> Attribute(msg) {
  attribute("alt", text)
}

// AUDIO AND VIDEO -------------------------------------------------------------

///
pub fn autoplay(should_autoplay: Bool) -> Attribute(msg) {
  property("autoplay", should_autoplay)
}

///
pub fn controls(visible: Bool) -> Attribute(msg) {
  property("controls", visible)
}

///
pub fn loop(should_loop: Bool) -> Attribute(msg) {
  property("loop", should_loop)
}
