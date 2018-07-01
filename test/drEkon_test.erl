-module(drEkon_test).
-include_lib("eunit/include/eunit.hrl").

-define(TESTS, [
{"hello", "hello.json",
 {drakon, hello, ["John", "Doe"]}, "Hello John Doe!\n"}
]).

parse_test_() ->
    {ok, Pwd} = file:get_cwd(),
    {inparallel,
        [{Title,
            fun() ->
				{ok, JsonBin} = file:read_file(filename:join([Pwd, "test", JsonFile])),
                JsonMap = jsx:decode(JsonBin, [return_maps]),
                ?assertEqual(true, is_map(JsonMap)),
                Tid = drEkon:load(JsonMap),
                {ok, Mod, ModuleStr} = drEkon:generate(Tid, Mod),
                {ok, Bin} = drEkon:compile_module(Mod, ModuleStr),
                {module, Mod} = code:load_binary(Mod, "nofile", Bin),
                ?assertEqual(Result, apply(Mod, Fun, Args))
            end}
        || {Title, JsonFile, {Mod, Fun, Args}, Result} <- ?TESTS]
    }.