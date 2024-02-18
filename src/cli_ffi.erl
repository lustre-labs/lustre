-module(cli_ffi).
-export([
    get_cpu/0,
    get_esbuild/1,
    get_os/0,
    unzip_esbuild/1
]).

get_os() ->
    case os:type() of
        {win32, _} -> <<"win32">>;
        {unix, darwin} -> <<"darwin">>;
        {unix, linux} -> <<"linux">>;
        {_, Unknown} -> atom_to_binary(Unknown, utf8)
    end.

get_cpu() ->
    case erlang:system_info(os_type) of
        {unix, _} ->
            [Arch, _] = string:split(erlang:system_info(system_architecture), "-"),
            list_to_binary(Arch);
        {win32, _} ->
            case erlang:system_info(wordsize) of
                4 -> {ok, <<"ia32">>};
                8 -> {ok, <<"x64">>}
            end
    end.

get_esbuild(Url) ->
    inets:start(),
    ssl:start(),

    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Zip}} -> {ok, Zip};
        {ok, Res} -> {error, Res};
        {error, Err} -> {error, Err}
    end.

unzip_esbuild(Zip) ->
    Result =
        erl_tar:extract({binary, Zip}, [
            memory, compressed, {files, ["package/bin/esbuild"]}
        ]),

    case Result of
        {ok, [{_, Esbuild}]} -> {ok, Esbuild};
        {ok, Res} -> {error, Res};
        {error, Err} -> {error, Err}
    end.
