import gleam/io
import lustre
import lustre/attribute.{ Attribute, attribute }
import lustre/element.{ Element }
import playground/monaco

pub type Action {
    OnInput(String)
}

pub fn main () {
    let init = "// Write some Gleam code here"

    let update = fn (_, action) {
        case action {
            OnInput(input) ->
                io.debug(input)
        }
    }

    let render = fn (state) {
        monaco.render([
            attribute("value", state),
            monaco.on_change(fn (code, dispatch) {
                dispatch(OnInput(code))
            })
        ])
    }

    lustre.simple(init, update, render)
        |> lustre.start("[data-lustre-container]")
}
