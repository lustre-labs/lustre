////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{ Dynamic }
import lustre/attribute.{ Attribute }

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn on (name: String, handler: fn (Dynamic, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    attribute.event(name, handler)
}

///
pub fn dispatch (action: action) -> fn (fn (action) -> Nil) -> Nil {
    fn (dispatch) {
        dispatch(action)
    }
}

// MOUSE EVENTS ----------------------------------------------------------------

///
pub fn on_click (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("click", fn (_, dispatch) { handler(dispatch) })
}

///
pub fn on_mouse_down (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mousedown", fn (_, dispatch) { handler(dispatch) })
}

///
pub fn on_mouse_up (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseup", fn (_, dispatch) { handler(dispatch) })
}

///
pub fn on_mouse_enter (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseenter", fn (_, dispatch) { handler(dispatch) })
}

///
pub fn on_mouse_leave (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseleave", fn (_, dispatch) { handler(dispatch) })
}

///
pub fn on_mouse_over (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseover", fn (_, dispatch) { handler(dispatch) })
}

///
pub fn on_mouse_out (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("mouseout", fn (_, dispatch) { handler(dispatch) })
}

// KEYBOARD EVENTS -------------------------------------------------------------

pub fn on_keypress (handler: fn (String, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("keypress", fn (e, dispatch) {
        assert Ok(key) = e |> dynamic.field("key", dynamic.string)

        handler(key, dispatch)
    })
}

pub fn on_keydown (handler: fn (String, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("keydown", fn (e, dispatch) {
        assert Ok(key) = e |> dynamic.field("key", dynamic.string)

        handler(key, dispatch)
    })
}

pub fn on_keyup (handler: fn (String, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("keyup", fn (e, dispatch) {
        assert Ok(key) = e |> dynamic.field("key", dynamic.string)

        handler(key, dispatch)
    })
}

// FORM EVENTS -----------------------------------------------------------------

///
pub fn on_input (handler: fn (String, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("input", fn (e, dispatch) {
        assert Ok(value) = e |> dynamic.field("target", dynamic.field("value", dynamic.string))

        handler(value, dispatch)
    })
}

pub fn on_check (handler: fn (Bool, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("check", fn (e, dispatch) {
        assert Ok(value) = e |> dynamic.field("target", dynamic.field("checked", dynamic.bool))

        handler(value, dispatch)
    })
}

pub fn on_submit (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("submit", fn (_, dispatch) { handler(dispatch) })
}

// FOCUS EVENTS ----------------------------------------------------------------

pub fn on_focus (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("focus", fn (_, dispatch) { handler(dispatch) })
}

pub fn on_blur (handler: fn (fn (action) -> Nil) -> Nil) -> Attribute(action) {
    on("blur", fn (_, dispatch) { handler(dispatch) })
}
