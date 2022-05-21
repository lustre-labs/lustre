// IMPORTS ---------------------------------------------------------------------

import lustre
import lustre/cmd.{ Cmd }
import lustre/element.{ Element }

import example/application/counter

// MAIN ------------------------------------------------------------------------

pub fn main () -> Nil {
    let selector = "[data-lustre-container]"
    let program  = lustre.application(init(), update, render)

    // `lustre.start` can return an `Error` if no DOM element is found that matches
    // the selector. This is a fatal error for our examples, so we panic if that 
    // happens.
    assert Ok(dispatch) = lustre.start(program, selector)

    dispatch(Counter(counter.Incr))
    dispatch(Counter(counter.Incr))
    dispatch(Counter(counter.Incr))

    Nil
}

// STATE -----------------------------------------------------------------------

type State {
    State(
        counter: counter.State
    )
}

fn init () -> #(State, Cmd(Action)) {
    let #(counter, counter_cmds) = counter.init()
    let state = State(
        counter
    )

    #(state, cmd.map(counter_cmds, Counter))
}

// UPDATE ----------------------------------------------------------------------

type Action {
    Counter(counter.Action)
}

fn update (state: State, action: Action) -> #(State, Cmd(Action)) {
    case action {
        Counter(counter_action) -> {
            let #(counter, counter_cmds) = counter.update(state.counter, counter_action)
            let state = State(
                counter
            )

            #(state, cmd.map(counter_cmds, Counter))
        }
    }
}

// RENDER ----------------------------------------------------------------------

fn render (state: State) -> Element(Action) {
    element.div([], [
        element.details([], [
            element.summary([], [ element.text("Counter") ]),
            counter.render(state.counter) 
                |> element.map(Counter)
        ])
    ])
}
