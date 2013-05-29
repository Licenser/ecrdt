-module(v2pset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, add/2, remove/2, merge/2, value/1, from_list/1, gc/1]).

-record(v2pset, {adds, removes}).

-opaque v2pset() :: #v2pset{}.

-export_type([v2pset/0]).

new() ->
    #v2pset{adds = vgset:new(),
            removes = vgset:new()}.

-spec from_list(list()) -> v2pset().
from_list(L) ->
    #v2pset{adds = vgset:from_list(L),
            removes = vgset:new()}.

-spec add(Element::term(), V2pset::v2pset()) -> V2pset1::v2pset().
add(Element, V2pset = #v2pset{adds = Adds}) ->
    V2pset#v2pset{adds = vgset:add(Element, Adds)}.

-spec remove(Element::term(), V2pset::v2pset()) -> V2pset1::v2pset().
remove(Element, V2pset = #v2pset{removes = Removes}) ->
    V2pset#v2pset{removes = vgset:add(Element, Removes)}.

-spec value(V2pset::v2pset()) -> [Element::term()].
value(#v2pset{adds = Adds,
              removes = Removes}) ->
    ordsets:subtract(vgset:value(Adds), vgset:value(Removes)).

-spec merge(V2pset0::v2pset(), V2pset1::v2pset()) -> V2psetM::v2pset().
merge(#v2pset{adds = Adds0,
              removes = Removes0},
      #v2pset{adds = Adds1,
              removes = Removes1}) ->
    #v2pset{adds = vgset:merge(Adds0, Adds1),
            removes = vgset:merge(Removes0, Removes1)}.

-spec gc(V2pset::v2pset()) -> V2psetGCed::v2pset().
gc(V2pset) ->
    #v2pset{adds = value(V2pset),
            removes = vgset:new()}.

-ifdef(TEST).

op(a, add, E, C1, C2, Check) ->
    {add(E, C1), C2, add(E, Check)};
op(b, add, E, C1, C2, Check) ->
    {C1, add(E, C2), add(E, Check)};
op(ab, add, E, C1, C2, Check) ->
    {add(E, C1), add(E, C2), add(E, Check)};
op(a, remove, E, C1, C2, Check) ->
    {remove(E, C1), C2, remove(E, Check)};
op(b, remove, E, C1, C2, Check) ->
    {C1, remove(E, C2), remove(E, Check)};
op(ab, remove, E, C1, C2, Check) ->
    {remove(E, C1), remove(E, C2), remove(E, Check)}. %;

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, E}, {A, B, C}) ->
                        op(T, O, E, A, B, C)
                end, {new(), new(), new()}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), oneof([add, remove]), pos_integer()}).

prop_v2pset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
