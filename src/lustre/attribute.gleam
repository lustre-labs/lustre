// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{ Dynamic }
import gleam/int
import gleam/list
import gleam/pair
import gleam/string

// TYPES -----------------------------------------------------------------------

///
pub opaque type Attribute(action) {
    Attribute(name: String, value: String)
    Property(name: String, value: Dynamic)
    Event(name: String, handler: fn (Dynamic, fn (action) -> Nil) -> Nil)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn attribute (name: String, value: String) -> Attribute(action) {
    Attribute(name, value)
}

///
pub fn property (name: String, value: Dynamic) -> Attribute(action) {
    Property(name, value)
}

///
pub fn event (name: String, handler: fn (Dynamic, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    Event(name, handler)
}

// COMMON ATTRIBUTES -----------------------------------------------------------

///
pub fn style (properties: List(#(String, String))) -> Attribute(action) {
    property("style", style_object(properties))
}

external fn style_object (properties: List(#(String, String))) -> Dynamic
    = "../ffi.mjs" "object"

///
pub fn class (name: String) -> Attribute(action) {
    attribute("className", name)
}

///
pub fn classes (names: List(#(String, Bool))) -> Attribute(action) {
    attribute("className",
        names 
            |> list.filter(pair.second) 
            |> list.map(pair.first)
            |> string.join(" ")
    )
}

///
pub fn id (name: String) -> Attribute(action) {
    attribute("id", name)
}

// INPUTS ----------------------------------------------------------------------

///
pub fn type_ (name: String) -> Attribute(action) {
    attribute("type", name)
}

///
pub fn value (val: Dynamic) -> Attribute(action) {
    property("value", val)
}

///
pub fn checked (is_checked: Bool) -> Attribute(action) {
    property("checked", dynamic.from(is_checked))
}

///
pub fn placeholder (text: String) -> Attribute(action) {
    attribute("placeholder", text)
}

///
pub fn selected (is_selected: Bool) -> Attribute(action) {
    property("selected", dynamic.from(is_selected))
}

// INPUT HELPERS ---------------------------------------------------------------

///
pub fn accept (types: List(String)) -> Attribute(action) {
    attribute("accept", string.join(types, " "))
}

///
pub fn accept_charset (types: List(String)) -> Attribute(action) {
    attribute("acceptCharset", string.join(types, " "))
}

///
pub fn action (uri: String) -> Attribute(action) {
    attribute("action", uri)
}

///
pub fn autocomplete (should_autocomplete: Bool) -> Attribute(action) {
    property("autocomplete", dynamic.from(should_autocomplete))
}

///
pub fn autofocus (should_autofocus: Bool) -> Attribute(action) {
    property("autofocus", dynamic.from(should_autofocus))
}

///
pub fn disabled (is_disabled: Bool) -> Attribute(action) {
    property("disabled", dynamic.from(is_disabled))
}

///
pub fn name (name: String) -> Attribute(action) {
    attribute("name", name)
}

///
pub fn pattern (regex: String) -> Attribute(action) {
    attribute("pattern", regex)
}

///
pub fn readonly (is_readonly: Bool) -> Attribute(action) {
    property("readonly", dynamic.from(is_readonly))
}

///
pub fn require (is_required: Bool) -> Attribute(action) {
    property("required", dynamic.from(is_required))
}

///
pub fn for (id: String) -> Attribute(action) {
    attribute("for", id)
}

// INPUT RANGES ----------------------------------------------------------------

///
pub fn max (val: String) -> Attribute(action) {
    attribute("max", val)
}

///
pub fn min (val: String) -> Attribute(action) {
    attribute("min", val)
}

///
pub fn step (val: String) -> Attribute(action) {
    attribute("step", val)
}

// INPUT TEXT AREAS ------------------------------------------------------------

///
pub fn cols (val: Int) -> Attribute(action) {
    attribute("cols", int.to_string(val))
}

///
pub fn rows (val: Int) -> Attribute(action) {
    attribute("rows", int.to_string(val))
}

///
pub fn wrap (mode: String) -> Attribute(action) {
    attribute("wrap", mode)
}

// LINKS AND AREAS -------------------------------------------------------------

///
pub fn href (uri: String) -> Attribute(action) {
    attribute("href", uri)
}

///
pub fn target (target: String) -> Attribute(action) {
    attribute("target", target)
}

///
pub fn download (filename: String) -> Attribute(action) {
    attribute("download", filename)
}

///
pub fn rel (relationship: String) -> Attribute(action) {
    attribute("rel", relationship)
}

// EMBEDDED CONTENT ------------------------------------------------------------

///
pub fn src (uri: String) -> Attribute(action) {
    attribute("src", uri)
}

///
pub fn height (val: Int) -> Attribute(action) {
    attribute("height", int.to_string(val))
}

///
pub fn width (val: Int) -> Attribute(action) {
    attribute("width", int.to_string(val))
}

///
pub fn alt (text: String) -> Attribute(action) {
    attribute("alt", text)
}

// AUDIO AND VIDEO -------------------------------------------------------------

///
pub fn autoplay (should_autoplay: Bool) -> Attribute(action) {
    property("autoplay", dynamic.from(should_autoplay))
}

///
pub fn controls (visible: Bool) -> Attribute(action) {
    property("controls", dynamic.from(visible))
}

///
pub fn loop (should_loop: Bool) -> Attribute(action) {
    property("loop", dynamic.from(should_loop))
}
