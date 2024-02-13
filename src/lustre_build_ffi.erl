-module(lustre_build_ffi).
-export([
    exec/1
]).

exec(Cmd) ->
    Stdout = os:cmd(unicode:characters_to_list(Cmd)),

    unicode:characters_to_binary(Stdout).
