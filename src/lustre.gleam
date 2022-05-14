////


// IMPORTS ---------------------------------------------------------------------

import lustre/element
import lustre/attribute


// TYPES -----------------------------------------------------------------------

///
pub opaque type Program(state, action) {
    Program(
        init: state,
        update: Update(state, action),
        render: Render(state, action)
    )
}


pub type Element(action)   = element.Element(action)
pub type Attribute(action) = attribute.Attribute(action)


// These types aren't exposed, but they're just here to try and shrink the type
// annotations for `Program` and `program` a little bit. When generating docs,
// Gleam automatically expands type aliases so this is purely for the benefit of
// those reading the source.
//
type Update(state, action) = fn (state, action) -> state
type Render(state, action) = fn (state) -> Element(action)


// CONSTRUCTORS ----------------------------------------------------------------

/// Create a basic lustre program that just renders some element on the page.
/// Note that this doesn't mean the content is static! With `element.stateful`
/// you can still create components with local state.
///
/// Basic lustre programs don't have any *global* application state and so the
/// plumbing is a lot simpler. If you find yourself passing state 
///
pub fn basic (element: Element(any)) -> Program(Nil, any) {
    let init   = Nil
    let update = fn (_, _) { Nil }
    let render = fn (_) { element }

    Program(init, update, render)
}

/// Create a more complex application mimicing TEA – the Elm architecture. We
/// start with some initial `state`, a function to update that state, and then
/// a render function to derive our program's view from that state.
///
/// 
pub fn application (init: state, update: Update(state, action), render: Render(state, action)) -> Program(state, action) {
    Program(init, update, render)
}


// EFFECTS ---------------------------------------------------------------------

///
pub external fn start (program: Program(state, action), selector: String) -> Nil
    = "./lustre/ffi.mjs" "mount"
