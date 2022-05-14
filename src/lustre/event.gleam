import gleam/dynamic.{ Dynamic }
import lustre/attribute.{ Attribute }

pub external fn ignore () -> action
    = "../lustre.mjs" "ignore"


pub fn on (name: String, handler: fn (Dynamic, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    attribute.event(name, handler)
}


// MOUSE EVENTS ----------------------------------------------------------------

pub fn on_click (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("click", fn (_, dispatch) { handler(dispatch) })
}

pub fn on_mouse_down (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseDown", fn (_, dispatch) { handler(dispatch) })
}

pub fn on_mouse_up (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseUp", fn (_, dispatch) { handler(dispatch) })
}

pub fn on_mouse_enter (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseEnter", fn (_, dispatch) { handler(dispatch) })
}

pub fn on_mouse_leave (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseLeave", fn (_, dispatch) { handler(dispatch) })
}

pub fn on_mouse_over (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseOver", fn (_, dispatch) { handler(dispatch) })
}

pub fn on_mouse_out (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseOut", fn (_, dispatch) { handler(dispatch) })
}

// FORM EVENTS -----------------------------------------------------------------

pub fn on_input (handler: fn (String, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("input", fn (e, dispatch) {
        assert Ok(value) = e |> dynamic.field("target", dynamic.field("value", dynamic.string))

        handler(value, dispatch)
    })
}
