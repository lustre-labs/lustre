// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

/// A `Cmd` represents some side effect we want the Lustre runtime to perform.
/// It is parameterised by our app's `action` type because some effects need to
/// get information back into your program.
///
pub opaque type Cmd(action) {
  Cmd(fn(fn(action) -> Nil) -> Nil, Cmd(action))
  None
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
  Cmd(cmd, None)
}

/// Typically our app's `update` function needs to return a tuple of
/// `#(model, Cmd(action))`. When we don't need to perform any side effects we
/// can just return `none()`!
///
pub fn none() -> Cmd(action) {
  None
}

// MANIPULATIONS ---------------------------------------------------------------

/// 
///
pub fn batch(cmds: List(Cmd(action))) -> Cmd(action) {
  cmds
  |> list.flat_map(to_list)
  |> list.fold_right(None, fn(rest, cmd) { Cmd(cmd, rest) })
}

pub fn map(cmd: Cmd(a), f: fn(a) -> b) -> Cmd(b) {
  case cmd {
    Cmd(cmd, next) ->
      Cmd(fn(dispatch) { cmd(fn(a) { dispatch(f(a)) }) }, map(next, f))

    None -> None
  }
}

// CONVERSIONS -----------------------------------------------------------------

pub fn to_list(cmd: Cmd(action)) -> List(fn(fn(action) -> Nil) -> Nil) {
  case cmd {
    Cmd(cmd, next) -> [cmd, ..to_list(next)]

    None -> []
  }
}
