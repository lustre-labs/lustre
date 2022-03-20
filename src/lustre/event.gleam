import gleam/dynamic.{ Dynamic }
import lustre/attribute.{ Attribute }

pub fn on (event: String, handler: fn (Dynamic) -> action) -> Attribute(action) {
    attribute.event(event, handler)
}

//

pub fn on_click (handler: action) -> Attribute(action) {
    on("onClick", fn (_) { handler })
}

pub fn on_input (handler: fn (String) -> action) -> Attribute(action) {
    let decoder = dynamic.field("target", dynamic.field("value", dynamic.string))

    on("onInput", fn (e) {
        // If this fails then there's probably some sort of hideous browser bug
        // that is way beyond the concern of our Lustre apps!
        assert Ok(value) = decoder(e)

        handler(value)
    })
}