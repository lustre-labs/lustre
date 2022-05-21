////


// IMPORTS ---------------------------------------------------------------------

import lustre/cmd.{ Cmd }
import lustre/element.{ Element }
import gleam/result
 

// TYPES -----------------------------------------------------------------------

/// An `App` describes a Lustre application: what state it holds and what kind
/// of actions get dispatched to update that state. The only useful thing you can
/// do with an `App` is pass it to [`start`](#start).
///
/// You can construct an `App` from the two constructors exposed in this module:
/// [`basic`](#basic) and [`application`](#application). Although you can't do
/// anything but [`start`](#start) them, the constructors are separated in case
/// you want to set up an application but defer starting it until some later point
/// in time.
///
///```
///                                      ┌────────┐
///                                      │        │
///                                      │ update │
///                                      │        │
///                                      └──────┬─┘
///                                        ▲    │
///                                        │    │ #(State, Action)
///                                 Action │    │
///                                        │    │
///                                        │    ▼
///  ┌──────┐                    ┌─────────┴──────────────┐
///  │      │  #(State, Action)  │                        │
///  │ init ├───────────────────►│     Lustre Runtime     │
///  │      │                    │                        │
///  └──────┘                    └──────────────┬─────────┘
///                                        ▲    │
///                                        │    │ State
///                                 Action │    │
///                                        │    ▼
///                                      ┌─┴──────┐
///                                      │        │
///                                      │ render │
///                                      │        │
///                                      └────────┘
///```
///
/// <small>Someone please PR the Gleam docs generator to fix the monospace font,
/// thanks! 💖</small>
///
pub opaque type App(state, action) {
    App(
        init: #(state, Cmd(action)),
        update: Update(state, action),
        render: Render(state, action)
    )
}

pub type Error {
    ElementNotFound
}

// These types aren't exposed, but they're just here to try and shrink the type
// annotations for `App` and `application` a little bit. When generating docs,
// Gleam automatically expands type aliases so this is purely for the benefit of
// those reading the source.
//
type Update(state, action) = fn (state, action) -> #(state, Cmd(action))
type Render(state, action) = fn (state) -> Element(action)


// CONSTRUCTORS ----------------------------------------------------------------

/// Create a basic lustre app that just renders some element on the page.
/// Note that this doesn't mean the content is static! With `element.stateful`
/// you can still create components with local state.
///
/// Basic lustre apps don't have any *global* application state and so the
/// plumbing is a lot simpler. If you find yourself passing lot's state of state
/// around, you might want to consider using `application` instead.
///
pub fn basic (element: Element(any)) -> App(Nil, any) {
    let init   = #(Nil, cmd.none())
    let update = fn (_, _) { #(Nil, cmd.none()) }
    let render = fn (_) { element }

    App(init, update, render)
}

/// Create a more complex application mimicing TEA – the Elm architecture. We
/// start with some initial `state`, a function to update that state, and then
/// a render function to derive our app's view from that state.
///
/// Events produced by elements are passed a `dispatch` function that can be 
/// used to emit actions that trigger your `update` function to be called and
/// trigger a rerender.
///
pub fn application (init: #(state, Cmd(action)), update: Update(state, action), render: Render(state, action)) -> App(state, action) {
    App(init, update, render)
}


// EFFECTS ---------------------------------------------------------------------

/// Once you have created a app with either `basic` or `application`, you 
/// need to actually start it! This function will mount your app to the DOM
/// node that matches the query selector you provide.
///
/// If everything mounted OK, we'll get back a dispatch function that you can 
/// call to send actions to your app and trigger an update. 
///
pub fn start (app: App(state, action), selector: String) -> Result(fn (action) -> Nil, Error) {
    mount(app, selector)
        |> result.replace_error(ElementNotFound)
}


external fn mount (app: App(state, action), selector: String) -> Result(fn (action) -> Nil, Nil)
    = "./ffi.mjs" "mount"
