// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/attribute.{ style }
import lustre/element.{ button, div, p, text }
import lustre/event.{ dispatch, on_click }

// MAIN ------------------------------------------------------------------------

pub fn main () -> Nil {
    let selector = "[data-lustre-container]"
    let program  = lustre.application(init(), update, render)

    // `lustre.start` can return an `Error` if no DOM element is found that matches
    // the selector. This is a fatal error for our examples, so we panic if that 
    // happens.
    assert Ok(_) = lustre.start(program, selector)

    Nil
}

// STATE -----------------------------------------------------------------------

pub type State 
    = Int

pub fn init () -> State {
    0
}

// UPDATE ----------------------------------------------------------------------

pub type Action {
    Incr
    Decr
}

pub fn update (state, action) {
    case action {
        Incr ->
            state + 1

        Decr ->
            state - 1
    }
}

// RENDER ----------------------------------------------------------------------

pub fn render (state) {
    div([ style([ #("display", "flex") ]) ], [
        button([ on_click(dispatch(Decr)) ], [ text("-") ]),
        p([], [ int.to_string(state) |> text ]),
        button([ on_click(dispatch(Incr)) ], [ text("+") ])
    ])
}
