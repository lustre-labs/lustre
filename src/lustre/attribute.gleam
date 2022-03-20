import gleam/dynamic.{ Dynamic }

pub opaque type Attribute(action) {
    Attribute(name: String, value: String)
    Property(name: String, value: Dynamic)
    Event(on: String, handler: fn (Dynamic) -> action)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn attribute (name: String, value: String) -> Attribute(action) {
    Attribute(name, value)
}

pub fn property (name: String, value: Dynamic) -> Attribute(action) {
    Property(name, value)
}

pub fn event (on: String, handler: fn (Dynamic) -> action) -> Attribute(action) {
    Event(on, handler)
}