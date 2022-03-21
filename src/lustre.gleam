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
        render: View(state, action)
    )
}


///
pub type Element(action) = element.Element(action)
///
pub type Attribute(action) = attribute.Attribute(action)


/// 
type Update(state, action) = fn (state, action) -> state
type Render(state, action) = fn (state) -> Element(action)


// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn program (init: state, update: Update(state, action), render: Render(state, action)) -> Program(state, action) {
    Program(init, update, render)
}


// EFFECTS ---------------------------------------------------------------------

///
pub external fn start (program: Program(state, action), selector: String) -> Nil
    = "./lustre/ffi.mjs" "mount"
