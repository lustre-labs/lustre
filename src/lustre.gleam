//// Lustre is a declarative framework for building Web apps in Gleam. 


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
///                                      +--------+
///                                      |        |
///                                      | update |
///                                      |        |
///                                      +--------+
///                                        ^    |
///                                        |    | 
///                                 Action |    | #(State, Action)
///                                        |    |
///                                        |    v
///  +------+                    +------------------------+
///  |      |  #(State, Action)  |                        |
///  | init |------------------->|     Lustre Runtime     |
///  |      |                    |                        |
///  +------+                    +------------------------+
///                                        ^    |
///                                        |    |
///                                 Action |    | State
///                                        |    |
///                                        |    v
///                                      +--------+
///                                      |        |
///                                      | render |
///                                      |        |
///                                      +--------+
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
/// plumbing is a lot simpler. If you find yourself passing lots of state around,
/// you might want to consider using [`simple`](#simple) or [`application`](#application)
/// instead.
///
///```gleam
///import lustre
///import lustre/element
///
///pub fn main () {
///    let app = lustre.element(
///        element.h1([], [
///            element.text("Hello, world!") 
///        ])
///    )
///
///    lustre.start(app, "#root")
///}
///```
///
pub fn element (element: Element(any)) -> App(Nil, any) {
    let init   = #(Nil, cmd.none())
    let update = fn (_, _) { #(Nil, cmd.none()) }
    let render = fn (_) { element }

    App(init, update, render)
}

/// If you start off with a simple `[element`](#element) app, you may find
/// yourself leaning on [`stateful`](./lustrel/element.html#stateful) elements
/// to manage state used throughout your app. If that's the case or if you know
/// you need some global state from the get-go, you might want to construct a 
/// [`simple`](#simple) app instead.
///
/// This is one app constructor that allows your HTML elements to dispatch actions
/// to update your program state. 
///
///```
///import gleam/int
///import lustre
///import lustre/element
///import lustre/event.{ dispatch }
///
///type Action {
///    Incr
///    Decr
///}
///
///pub fn main () {
///    let init = 0
///    let update = fn (state, action) {
///        case action {
///            Incr -> state + 1
///            Decr -> state - 1
///        }
///    }
///    let render = fn (state) {
///        element.div([], [
///            element.button([ event.on_click(dispatch(Decr)) ], [
///                element.text("-")
///            ]),
///            element.text(state |> int.to_string |> element.text),
///            element.button([ event.on_click(dispatch(Incr)) ], [
///                element.text("+")
///            ])
///        ])
///    }
///
///    let app = lustre.simple(init, update, render)
///    lustre.start(app, "#root")
///}
///```
pub fn simple (init: state, update: fn (state, action) -> state, render: fn (state) -> Element(action)) -> App(state, action) {
    let init   = #(init, cmd.none())
    let update = fn (state, action) { #(update(state, action), cmd.none()) }

    App(init, update, render)
}

/// An evolution of a [`simple`](#simple) app that allows you to return a
/// [`Cmd`](./lustre/cmd.html#Cmd) from your `init` and `update`s. Commands give
/// us a way to perform side effects like sending an HTTP request or running a
/// timer and then dispatch actions back to the runtime to trigger an `update`.
///
///```
///import lustre
///import lustre/cmd
///import lustre/element
///
///pub fn main () {
///    let init = #(0, tick())
///    let update = fn (state, action) {
///        case action {
///            Tick -> #(state + 1, tick())
///        }
///    }
///    let render = fn (state) {
///        element.div([], [
///            element.text("Count is: ")
///            element.text(state |> int.to_string |> element.text)
///        ])
///    }
///
///    let app = lustre.simple(init, update, render)
///    lustre.start(app, "#root")   
///}
///
///fn tick () -> Cmd(Action) {
///    cmd.from(fn (dispatch) {
///        setInterval(fn () {
///            dispatch(Tick)
///        }, 1000)
///    })
///}
///
///external fn set_timeout (f: fn () -> a, delay: Int) -> Nil 
///    = "" "window.setTimeout"
///```
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
///```
///import lustre
///
///pub fn main () {
///    let app = lustre.appliation(init, update, render)
///    assert Ok(dispatch) = lustre.start(app, "#root")
///
///    dispatch(Incr)
///    dispatch(Incr)
///    dispatch(Incr)
///}
///```
///
pub fn start (app: App(state, action), selector: String) -> Result(fn (action) -> Nil, Error) {
    mount(app, selector)
        |> result.replace_error(ElementNotFound)
}


external fn mount (app: App(state, action), selector: String) -> Result(fn (action) -> Nil, Nil)
    = "./ffi.mjs" "mount"
