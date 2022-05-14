import gleam/dynamic.{ Dynamic }

pub opaque type Attribute(action) {
    Attribute(name: String, value: String)
    Property(name: String, value: Dynamic)
    Event(name: String, handler: fn (Dynamic, fn (action) -> Nil) -> Nil)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn attribute (name: String, value: String) -> Attribute(action) {
    Attribute(name, value)
}

pub fn property (name: String, value: Dynamic) -> Attribute(action) {
    Property(name, value)
}

pub fn event (name: String, handler: fn (Dynamic, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    Event(name, handler)
}

//

pub fn style (properties: List(#(String, String))) -> Attribute(action) {
    property("style", style_object(properties))
}

external fn style_object (properties: List(#(String, String))) -> Dynamic
    = "../ffi.mjs" "object"
