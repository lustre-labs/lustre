// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/attribute.{ style }
import lustre/cmd.{ Cmd }
import lustre/element.{ button, div, p, text }
import lustre/event.{ on_click }

// MAIN ------------------------------------------------------------------------

pub fn main () -> Nil {
    let selector = "[data-lustre-container]"
    let program  = lustre.application(init(), update, render)

    // `lustre.start` can return an `Error` if no DOM element is found that matches
    // the selector. This is a fatal error for our examples, so we panic if that 
    // happens.
    let assert Ok(_) = lustre.start(program, selector)

    Nil
}

// STATE -----------------------------------------------------------------------

pub type State {
    State(
        count: Int,
        clock: Bool,
        timer_id: Int
    )
}

pub fn init () -> #(State, Cmd(Action)) {
    #(State(0, True, 0), tick())
}

// UPDATE ----------------------------------------------------------------------

pub type Action {
    Incr
    Decr
    Tick
}

pub fn update (state, action) {
    case action {
        Incr ->
            #(State(..state, count: state.count + 1), cmd.none())

        Decr ->
            #(State(..state, count: state.count - 1), cmd.none())

        Tick ->
            #(State(..state, count: state.count + 1), 
                case state.clock {
                    True -> 
                        tick()

                    False ->
                        cmd.none()        
                }
            )
    }
}

external fn after (f: fn () -> any, delay: Int) -> Nil
    = "" "window.setTimeout"

fn tick () -> Cmd(Action) {
    cmd.from(fn (dispatch) {
        after(fn () {
            dispatch(Tick)
        }, 1000)
    })
}

// RENDER ----------------------------------------------------------------------

pub fn render (state: State) {
    div([ style([ #("display", "flex") ]) ], [
        button([ on_click(Decr) ], [ text("-") ]),
        p([], [ int.to_string(state.count) |> text ]),
        button([ on_click(Incr) ], [ text("+") ]),
    ])
}
