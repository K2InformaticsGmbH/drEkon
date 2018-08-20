%% -----------------------------------------------------------------------------
%%
%% drEkon.erl: Encoder / Decoder of DRAKON diagrams.
%%
%% Copyright (c) 2017-forever K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-module(drEkon).
-author("chatterjee@bluewin.ch").

-export([load/1, generate/2, compile_module/2]).

-export([make_graph/1, reduce_junctions/1]).

-ifdef(CONSOLE).
f().
{ok, JB} = file:read_file("test/udh_parser/public/pack.json").
J = jsx:decode(JB, [return_maps]).
G = drEkon:make_graph(J).
RG = drEkon:reduce_junctions(G).
RGB = jsx:encode(RG).
ok = file:write_file("test/udh_parser/public/pack_rg.json", RGB).
-endif.

make_graph(#{<<"nodes">> := Nodes, <<"edges">> := Edges}) ->
    maps:fold(
        fun(_, #{<<"head">> := HBin, <<"tail">> := TBin}, Graph) ->
            H = binary_to_integer(HBin),
            T = binary_to_integer(TBin),
            {Head, Tail, NewGraph} =
                case maps:with([H, T], Graph) of
                    #{H := Hd, T := Tl} -> {Hd, Tl, Graph};
                    #{H := Hd} ->
                        Tl = cleanup_node(maps:get(TBin, Nodes)),
                        {Hd, Tl, Graph#{T => Tl}};
                    #{T := Tl} ->
                        Hd = cleanup_node(maps:get(HBin, Nodes)),
                        {Hd, Tl, Graph#{H => Hd}};
                    _ ->
                        Hd = cleanup_node(maps:get(HBin, Nodes)),
                        Tl = cleanup_node(maps:get(TBin, Nodes)),
                        {Hd, Tl, Graph#{H => Hd, T => Tl}}
                end,
            NewGraph1 =
                case Head of
                    #{<<"neighbours">> := Neighbours} ->
                        NewGraph#{H => Head#{<<"neighbours">> => [T | Neighbours]}};
                    Head ->
                        NewGraph#{H => Head#{<<"neighbours">> => [T]}}
                end,
            case Tail of
                #{<<"neighbours">> := Neighbours1} ->
                    NewGraph1#{T => Tail#{<<"neighbours">> => [H | Neighbours1]}};
                Tail ->
                    NewGraph1#{T => Tail#{<<"neighbours">> => [H]}}
            end
        end, #{}, Edges
    ).

reduce_junctions(Graph) ->
    case
        maps:filter(
            fun(_, #{<<"type">> := <<"junction">>}) -> true;
               (_, _) -> false
            end,
        Graph)
    of
        Empty when map_size(Empty) =:= 0 -> Graph;
        Junctions ->
            maps:fold(fun reduce_junctions/3, Graph, Junctions)
    end.

reduce_junctions(JunctionId, _, Graph) ->
    #{JunctionId := #{<<"neighbours">> := JunctionNeighbours}} = Graph,
    lists:foldl(
        reduce_junctions(JunctionId, JunctionNeighbours),
        maps:without([JunctionId], Graph), JunctionNeighbours
    ).

reduce_junctions(JunctionId, JunctionNeighbours) ->
    fun(Id, Graph) ->
        #{Id := #{<<"neighbours">> := Neighbours} = Node} = Graph,
        Graph#{
            Id => Node#{
                <<"neighbours">> =>
                    lists:usort((Neighbours ++ JunctionNeighbours) -- [JunctionId, Id])
            }
        }
    end.

cleanup_node(#{<<"content">> := #{<<"txt">> := Txt,
                                  <<"txt2">> := Txt2}} = Node) ->
    maps:without([<<"w">>, <<"h">>, <<"x">>, <<"y">>, <<"id">>, <<"isLine">>],
                 Node#{<<"content">> => [Txt, Txt2]});
cleanup_node(Node) ->
    maps:without([<<"w">>, <<"h">>, <<"x">>, <<"y">>, <<"id">>, <<"isLine">>], Node).

-record(obj, {id, type, name, subtype, content, role, head, tail}).

load(Json) ->
    Tab = ets:new(tab, [public, ordered_set, {keypos, 2}]),
    load(Tab, Json),
    Tab.

load(Tab, #{<<"name">> := Name, <<"type">> := Type,
          <<"nodes">> := Nodes, <<"edges">> := Edges}) ->
    ets:insert_new(Tab, #obj{id = 0, type = Type, name = Name}),
    load(Tab, {nodes, Nodes}),
    load(Tab, {edges, Edges});
load(Tab, {nodes, Nodes}) ->
    maps:fold(fun(Id, V, _) -> load(Tab, {node, Id, V}) end, none, Nodes);
load(Tab, {edges, Edges}) ->
    maps:fold(fun(Id, V, _) -> load(Tab, {edge, Id, V}) end, none, Edges);
load(Tab, {node, Id, #{<<"type">> := Type, <<"content">> := #{<<"txt">> := Content}}}) ->
    ets:insert_new(Tab, #obj{id = binary_to_integer(Id), type = node, subtype = Type, content = Content});
load(Tab, {edge, Id, #{<<"head">> := Head, <<"tail">> := Tail}}) ->
    ets:insert_new(Tab, #obj{id = binary_to_integer(Id), type = edge,
                             head = binary_to_integer(Head),
                             tail = binary_to_integer(Tail)}).

generate(Tab, Mod) when is_atom(Mod) ->
    Module = io_lib:format("-module(~p).~n~n", [Mod]),
    [{Id, Fun}] = ets:select(Tab,
        [{#obj{id = '$1', type = node, subtype = <<"beginend">>, content = '$2', _ = '_'}, [], [{{'$1', '$2'}}]}]),
    NextIds = ets:select(Tab,
        [{#obj{type = edge, head = Id, tail = '$1', _ = '_'}, [], ['$1']}]),
    [#obj{id = ArgsId, content = <<"public\n\n", Arguments/binary>>} | _] =
        lists:filter(
            fun(#obj{id = I,
                    content = <<"public", _/binary>>}) ->
                    lists:member(I, NextIds);
                (_) -> false
            end,
            ets:select(Tab,
                [{#obj{type = node, subtype = <<"action">>, _ = '_'}, [], ['$_']}])),
    Args = [A || A <- re:split(Arguments, "\n", [{return, list}]), length(A) > 0],
    Module1 = Module ++ io_lib:format("-export([~s/~p]).~n~n", [Fun, length(Args)]),
    Module2 = Module1 ++ io_lib:format("~s(~s) ->~n", [Fun, string:join(Args, ", ")]),
    NextIds1 = NextIds -- [ArgsId],
    [#obj{id = ArgsId1, content = Content} | _] =
        lists:filter(
            fun(#obj{id = I,
                    content = <<_/binary>>}) ->
                    lists:member(I, NextIds1);
                (_) -> false
            end,
            ets:select(Tab,
                [{#obj{type = node, subtype = <<"action">>, _ = '_'}, [], ['$_']}])),
    Module3 = Module2 ++ io_lib:format("\t~s,~n", [Content]),
    NextIds2 = ets:select(Tab,
        [{#obj{type = edge, head = ArgsId1, tail = '$1', _ = '_'}, [], ['$1']}]),
    [#obj{id = ArgsId2, content = Content2} | _] =
        lists:filter(
            fun(#obj{id = I,
                    content = <<_/binary>>}) ->
                    lists:member(I, NextIds2);
                (_) -> false
            end,
            ets:select(Tab,
                [{#obj{type = node, subtype = <<"action">>, _ = '_'}, [], ['$_']}])),
    {ok, Mod, list_to_binary([Module3, io_lib:format("\t~s.~n", [Content2])])}.

compile_module(ModuleAtom, ModuleCodeBinStr) when is_binary(ModuleCodeBinStr) ->
    case erl_scan:string(binary_to_list(ModuleCodeBinStr)) of
        {ok, Tokens, _} ->
            TokenGroups = cut_dot(Tokens),
            case lists:foldl(
                    fun(TokenGroup, Acc) when is_list(Acc) ->
                            case erl_parse:parse_form(TokenGroup) of
                                {ok, AbsForm} -> [AbsForm | Acc];
                                {error, {_,_,ErrorDescriptor}} ->
                                    {error, {parse, ErrorDescriptor}}
                            end;
                        (_, Error) -> Error
                    end, [], TokenGroups) of
                Forms when is_list(Forms) ->
                    case compile:forms(Forms) of
                        error -> {error, compile};
                        {ok, ModuleAtom, Bin} -> {ok, Bin};
                        {ok, ModuleAtom, Bin, Warnings} ->
                            {warning, Bin, Warnings};
                        {error, Errors, Warnings} ->
                            {error, {compile, Errors, Warnings}}
                    end;
                Error -> Error
            end;
        {error, {_, _, ErrorDescriptor}, ErrorLocation} ->
            {error, {scan, ErrorDescriptor, ErrorLocation}}
    end.

cut_dot(Tokens) -> cut_dot(Tokens, [[]]).
cut_dot([], [[]|Acc]) -> cut_dot([], Acc);
cut_dot([], Acc) -> Acc;
cut_dot([{dot,_} = Dot | Tokens], [A | Rest]) ->
    cut_dot(Tokens, [[], lists:reverse([Dot | A]) | Rest]);
cut_dot([T | Tokens], [A | Rest]) -> cut_dot(Tokens, [[T | A] | Rest]).