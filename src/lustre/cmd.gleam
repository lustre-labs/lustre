// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

/// A `Cmd` represents some side effect we want the Lustre runtime to perform.
/// It is parameterised by our app's `action` type because some effects need to
/// get information back into your program.
///
pub opaque type Cmd(action) {
  Cmd(List(fn(fn(action) -> Nil) -> Nil))
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create a `Cmd` from some custom side effect. This is mostly useful for 
/// package authors, or for integrating other libraries into your Lustre app.
///
/// We pass in a function that recieves a `dispatch` callback that can be used
/// to send messages to the Lustre runtime. We could, for example, create a `tick`
/// command that uses the `setTimeout` JavaScript API to send a message to the
/// runtime every second:
///
/// ```gleam
/// import lustre/cmd.{Cmd}
/// 
/// external fn set_interval(callback: fn() -> any, interval: Int) =
///   "" "window.setInterval"
/// 
/// pub fn every_second(msg: msg) -> Cmd(msg) {
///   use dispatch <- cmd.from
/// 
///   set_interval(fn() { dispatch(msg) }, 1000)
/// }
/// ```
///
pub fn from(cmd: fn(fn(action) -> Nil) -> Nil) -> Cmd(action) {
  Cmd([cmd])
}

/// Typically our app's `update` function needs to return a tuple of
/// `#(model, Cmd(action))`. When we don't need to perform any side effects we
/// can just return `none()`!
///
pub fn none() -> Cmd(action) {
  Cmd([])
}

// MANIPULATIONS ---------------------------------------------------------------

/// 
///
pub fn batch(cmds: List(Cmd(action))) -> Cmd(action) {
  Cmd({
    use b, Cmd(a) <- list.fold(cmds, [])
    list.append(b, a)
  })
}

pub fn map(cmd: Cmd(a), f: fn(a) -> b) -> Cmd(b) {
  let Cmd(l) = cmd
  Cmd(list.map(l, fn(cmd) { fn(dispatch) { cmd(fn(a) { dispatch(f(a)) }) } }))
}
