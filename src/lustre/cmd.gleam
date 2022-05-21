// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

pub opaque type Cmd(action) {
    Cmd(fn (fn (action) -> Nil) -> Nil, Cmd(action))
    None
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn from (cmd: fn (fn (action) -> Nil) -> Nil) -> Cmd(action) {
    Cmd(cmd, None)
}

pub fn none () -> Cmd(action) {
    None
}

// MANIPULATIONS ---------------------------------------------------------------

pub fn batch (cmds: List(Cmd(action))) -> Cmd(action) {
    cmds
        |> list.flat_map(to_list)
        |> list.fold_right(None, fn (rest, cmd) { Cmd(cmd, rest) })
}

pub fn map (cmd: Cmd(a), f: fn (a) -> b) -> Cmd(b) {
    case cmd {
        Cmd(cmd, next) ->
            Cmd(fn (dispatch) {
                cmd(fn (a) {
                    dispatch(f(a))
                })
            }, map(next, f))   

        None ->
            None
    }
}

// CONVERSIONS -----------------------------------------------------------------

pub fn to_list (cmd: Cmd(action)) -> List(fn (fn (action) -> Nil) -> Nil) {
    case cmd {
        Cmd(cmd, next) ->
            [ cmd, ..to_list(next) ]

        None ->
            []
    }
}
