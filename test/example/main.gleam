import gleam/int
import lustre
import lustre/attribute.{ style }
import lustre/element.{ button, div, p, text }
import lustre/event.{ dispatch, on_click }

pub fn main () {
    let selector = "[data-lustre-container]"
    let program  = lustre.application(0, update, render)

    lustre.start(program, selector)
}

type Action {
    Incr
    Decr
}

fn update (state, action) {
    case action {
        Incr ->
            state + 1

        Decr ->
            state - 1
    }
}

fn render (state) {
    div([ style([ #("display", "flex") ]) ], [
        button([ on_click(dispatch(Decr)) ], [ text("-") ]),
        p([], [ int.to_string(state) |> text ]),
        button([ on_click(dispatch(Incr)) ], [ text("+") ])
    ])
}
