import gleam/dynamic
import lustre/attribute.{ Attribute }
import lustre/element.{ Element }
import lustre/event

pub external fn render (attributes: List(Attribute(action))) -> Element(action)
    = "../playground.mjs" "monaco"

pub fn on_change (handler: fn (String, fn (action) -> Nil) -> Nil) -> Attribute(action) {
    event.on("change", fn (e, dispatch) {
        let assert Ok(code) = dynamic.string(e)

        handler(code, dispatch)
    })
}