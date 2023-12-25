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
  data
  |> json.to_string
  |> attribute("data-lustre-data", _)
}

///
/// 
pub fn include(properties: List(String)) -> Attribute(msg) {
  properties
  |> json.array(json.string)
  |> json.to_string
  |> attribute("data-lustre-include", _)
}
