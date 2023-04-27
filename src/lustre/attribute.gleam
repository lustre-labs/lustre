// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/list
import gleam/pair
import gleam/string

// TYPES -----------------------------------------------------------------------

/// Attributes are attached to specific elements. They're either key/value pairs
/// or event handlers.
///
pub opaque type Attribute(msg) {
  Attribute(name: String, value: Dynamic)
  Event(name: String, handler: fn(Dynamic, fn(msg) -> Nil) -> Nil)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn attribute(name: String, value: any) -> Attribute(msg) {
  Attribute(name, dynamic.from(value))
}

///                                                            
pub fn event(
  name: String,
  handler: fn(Dynamic, fn(msg) -> Nil) -> Nil,
) -> Attribute(msg) {
  Event(name, handler)
}

// COMMON ATTRIBUTES -----------------------------------------------------------

///
pub fn style(properties: List(#(String, String))) -> Attribute(msg) {
  attribute("style", style_object(properties))
}

external fn style_object(properties: List(#(String, String))) -> Dynamic =
  "../lustre.ffi.mjs" "to_object"

///
pub fn class(name: String) -> Attribute(msg) {
  attribute("className", name)
}

///
pub fn classes(names: List(#(String, Bool))) -> Attribute(msg) {
  attribute(
    "className",
    names
    |> list.filter(pair.second)
    |> list.map(pair.first)
    |> string.join(" "),
  )
}

///
pub fn id(name: String) -> Attribute(msg) {
  attribute("id", name)
}

// INPUTS ----------------------------------------------------------------------

///
pub fn type_(name: String) -> Attribute(msg) {
  attribute("type", name)
}

///
pub fn value(val: Dynamic) -> Attribute(msg) {
  attribute("value", val)
}

///
pub fn checked(is_checked: Bool) -> Attribute(msg) {
  attribute("checked", is_checked)
}

///
pub fn placeholder(text: String) -> Attribute(msg) {
  attribute("placeholder", text)
}

///
pub fn selected(is_selected: Bool) -> Attribute(msg) {
  attribute("selected", is_selected)
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
pub fn autocomplete(should_autocomplete: Bool) -> Attribute(msg) {
  attribute("autocomplete", should_autocomplete)
}

///
pub fn autofocus(should_autofocus: Bool) -> Attribute(msg) {
  attribute("autoFocus", should_autofocus)
}

///
pub fn disabled(is_disabled: Bool) -> Attribute(msg) {
  attribute("disabled", is_disabled)
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
  attribute("readonly", is_readonly)
}

///
pub fn required(is_required: Bool) -> Attribute(msg) {
  attribute("required", is_required)
}

///
pub fn for(id: String) -> Attribute(msg) {
  attribute("htmlFor", id)
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
  attribute("height", int.to_string(val))
}

///
pub fn width(val: Int) -> Attribute(msg) {
  attribute("width", int.to_string(val))
}

///
pub fn alt(text: String) -> Attribute(msg) {
  attribute("alt", text)
}

// AUDIO AND VIDEO -------------------------------------------------------------

///
pub fn autoplay(should_autoplay: Bool) -> Attribute(msg) {
  attribute("autoplay", should_autoplay)
}

///
pub fn controls(visible: Bool) -> Attribute(msg) {
  attribute("controls", visible)
}

///
pub fn loop(should_loop: Bool) -> Attribute(msg) {
  attribute("loop", should_loop)
}
