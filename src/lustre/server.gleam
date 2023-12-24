// IMPORTS ---------------------------------------------------------------------

import gleam/json.{type Json}
import lustre/attribute.{type Attribute, attribute}
import lustre/element.{type Element, element}

// ELEMENTS --------------------------------------------------------------------

///
/// 
pub fn component(attrs: List(Attribute(msg))) -> Element(msg) {
  element("lustre-server-component", attrs, [])
}

// ATTRIBUTES ------------------------------------------------------------------

/// 
/// 
pub fn route(path: String) -> Attribute(msg) {
  attribute("route", path)
}

///
/// 
pub fn data(data: Json) -> Attribute(msg) {
  attribute("data-server-data", json.to_string(data))
}

// EVENTS ----------------------------------------------------------------------

///
/// 
pub fn on(event: String, tag: String) -> Attribute(msg) {
  attribute("data-server-" <> event, tag)
}

///
/// 
pub fn on_click(tag: String) -> Attribute(msg) {
  attribute("data-server-click", tag)
}

///
/// 
pub fn on_input(tag: String) -> Attribute(msg) {
  attribute("data-server-input", tag)
}
