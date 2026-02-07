-module(effect_ffi).
-export([make_key/0]).

make_key() ->
    erlang:unique_integer([positive]).
