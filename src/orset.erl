-module(orset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, add/2, add/3, remove/2, merge/2, value/1, from_list/1, gc/1]).

-record(orset, {adds, removes}).

-opaque orset() :: #orset{}.

-export_type([orset/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

-spec new() -> orset().
new() ->
    #orset{adds = vgset:new(),
           removes = vgset:new()}.

-spec from_list(list()) -> orset().
from_list(L) ->
    ID = ecrdt:id(),
    #orset{adds = vgset:from_list([ {E, ID} || E <- L]),
           removes = vgset:new()}.

-spec add(ID::term(), Element::term(), ORSet::orset()) -> ORSet1::orset().
add(ID, Element, ORSet = #orset{adds = Adds}) ->
    ORSet#orset{adds = vgset:add({Element, ID}, Adds)}.

-spec add(Element::term(), ORSet::orset()) -> ORSet1::orset().
add(Element, ORSet = #orset{adds = Adds}) ->
    ORSet#orset{adds = vgset:add({Element, ecrdt:id()}, Adds)}.


-spec remove(Element::term(), ORSet::orset()) -> ORSet1::orset().
remove(Element, ORSet = #orset{removes = Removes}) ->
    CurrentExisting = [Elem || Elem = {E1, _} <- raw_value(ORSet), E1 =:= Element],
    Removes1 = lists:foldl(fun(R, Rs) ->
                                  vgset:add(R, Rs)
                          end, Removes, CurrentExisting),
    ORSet#orset{removes = Removes1}.

-spec merge(ORSet0::orset(), ORSet1::orset()) -> ORSetM::orset().
merge(#orset{adds = Adds0,
             removes = Removes0},
      #orset{adds = Adds1,
             removes = Removes1}) ->
    #orset{adds = vgset:merge(Adds0, Adds1),
           removes = vgset:merge(Removes0, Removes1)}.

-spec value(ORSet::orset()) -> [Element::term()].
value(ORSet) ->
    ordsets:from_list([E || {E, _} <- raw_value(ORSet)]).

-spec gc(ORSet::orset()) -> ORSetGCed::orset().
gc(ORSet) ->
    #orset{adds = raw_value(ORSet),
           removes = vgset:new()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_value(ORSet::orset()) -> [{Element::term(), ID::term()}].
raw_value(#orset{adds = Adds,
                 removes = Removes}) ->
    ordsets:subtract(vgset:value(Adds), vgset:value(Removes)).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, add, E, C1, C2, Check) ->
    ID = ecrdt:id(),
    {add(ID, E, C1), C2, add(ID, E, Check)};
op(b, add, E, C1, C2, Check) ->
    ID = ecrdt:id(),
    {C1, add(ID, E, C2), add(ID, E, Check)};
op(ab, add, E, C1, C2, Check) ->
    ID = ecrdt:id(),
    {add(ID, E, C1), add(ID, E, C2), add(ID, E, Check)};
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

prop_orset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
