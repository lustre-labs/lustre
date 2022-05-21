////


// IMPORTS ---------------------------------------------------------------------

import lustre/cmd
import lustre/element
import lustre/attribute
import gleam/result


// TYPES -----------------------------------------------------------------------

///
pub opaque type Program(state, action) {
    Program(
        init: #(state, Cmd(action)),
        update: Update(state, action),
        render: Render(state, action)
    )
}

pub type Cmd(action) = cmd.Cmd(action)

pub type Element(action)   = element.Element(action)
pub type Attribute(action) = attribute.Attribute(action)

pub type Error {
    ElementNotFound
}

// These types aren't exposed, but they're just here to try and shrink the type
// annotations for `Program` and `program` a little bit. When generating docs,
// Gleam automatically expands type aliases so this is purely for the benefit of
// those reading the source.
//
type Update(state, action) = fn (state, action) -> #(state, Cmd(action))
type Render(state, action) = fn (state) -> Element(action)


// CONSTRUCTORS ----------------------------------------------------------------

/// Create a basic lustre program that just renders some element on the page.
/// Note that this doesn't mean the content is static! With `element.stateful`
/// you can still create components with local state.
///
/// Basic lustre programs don't have any *global* application state and so the
/// plumbing is a lot simpler. If you find yourself passing lot's state of state
/// around, you might want to consider using `application` instead.
///
pub fn basic (element: Element(any)) -> Program(Nil, any) {
    let init   = #(Nil, cmd.none())
    let update = fn (_, _) { #(Nil, cmd.none()) }
    let render = fn (_) { element }

    Program(init, update, render)
}

/// Create a more complex application mimicing TEA – the Elm architecture. We
/// start with some initial `state`, a function to update that state, and then
/// a render function to derive our program's view from that state.
///
/// Events produced by elements are passed a `dispatch` function that can be 
/// used to emit actions that trigger your `update` function to be called and
/// trigger a rerender.
///
pub fn application (init: #(state, Cmd(action)), update: Update(state, action), render: Render(state, action)) -> Program(state, action) {
    Program(init, update, render)
}


// EFFECTS ---------------------------------------------------------------------

/// Once you have created a program with either `basic` or `application`, you 
/// need to actually start it! This function will mount your program to the DOM
/// node that matches the query selector you provide.
///
/// If everything mounted OK, we'll get back a dispatch function that you can 
/// call to send actions to your program and trigger an update. 
///
pub fn start (program: Program(state, action), selector: String) -> Result(fn (action) -> Nil, Error) {
    mount(program, selector)
        |> result.replace_error(ElementNotFound)
}


external fn mount (program: Program(state, action), selector: String) -> Result(fn (action) -> Nil, Nil)
    = "./ffi.mjs" "mount"
