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
        view: View(state, action)
    )
}


///
pub type Element(action) = element.Element(action)
///
pub type Attribute(action) = attribute.Attribute(action)


/// 
type Update(state, action) = fn (state, action) -> state
type View(state, action) = fn (state) -> Element(action)


// CONSTRUCTORS ----------------------------------------------------------------
///
pub fn create (init: state, update: Update(state, action), view: View(state, action)) -> Program(state, action) {
    Program(init, update, view)
}


///
pub external fn start (program: Program(state, action), selector: String) -> Nil
    = "./lustre/ffi.mjs" "mount"


// CONVERSIONS -----------------------------------------------------------------
///
pub external fn to_element (program: Program(state, action)) -> Element(a)
    = "./lustre/ffi.mjs" "Program"
