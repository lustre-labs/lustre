-module(cli_ffi).
-export([
    get_cpu/0,
    get_esbuild/1,
    get_tailwind/1,
    get_os/0,
    unzip_esbuild/1,
    exec/3
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

get_tailwind(Url) ->
    inets:start(),
    ssl:start(),

    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Bin}} -> {ok, Bin};
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

exec(Command, Args, Cwd) ->
    Command_ = binary_to_list(Command),
    Args_ = lists:map(fun(Arg) -> binary_to_list(Arg) end, Args),
    Cwd_ = binary_to_list(Cwd),

    Name = case Command_ of
      "./" ++ _ -> {spawn_executable, Command_};
      "/" ++ _ -> {spawn_executable, Command_};
      _ -> {spawn_executable, os:find_executable(Command_)}
    end,

    Port = open_port(Name, [exit_status, binary, hide, stream, eof,
        {args, Args_},
        {cd, Cwd_}
    ]),

    do_exec(Port, []).

do_exec(Port, Acc) ->
    receive
        {Port, {data, Data}} -> do_exec(Port, [Data | Acc]);
        {Port, {exit_status, 0}} -> {ok, list_to_binary(lists:reverse(Acc))};
        {Port, {exit_status, Code}} -> {error, {Code, list_to_binary(lists:reverse(Acc))}}
    end.
