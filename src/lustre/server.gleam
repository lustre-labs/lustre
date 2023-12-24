// IMPORTS ---------------------------------------------------------------------

import lustre/attribute.{type Attribute, attribute}

// EVENTS ----------------------------------------------------------------------

pub fn on_click(tag: String) -> Attribute(msg) {
  attribute("data-server-click", tag)
}

pub fn on_input(tag: String) -> Attribute(msg) {
  attribute("data-server-input", tag)
}
