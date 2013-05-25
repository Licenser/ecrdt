-module(orset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.


-export([new/0, add/2, add/3, remove/2, merge/2, value/1, id/0, from_list/1, gc/1]).

-record(orset, {adds, removes}).

new() ->
    #orset{adds = sgset:new(),
           removes = sgset:new()}.

from_list(L) ->
    ID = id(),
    #orset{adds = sgset:from_list([ {E, ID} || E <- L]),
           removes = sgset:new()}.

add(ID, E, ORSet = #orset{adds = Adds}) ->
    ORSet#orset{adds = sgset:add({E, ID}, Adds)}.

add(E, ORSet = #orset{adds = Adds}) ->
    ORSet#orset{adds = sgset:add({E, id()}, Adds)}.

remove(E, ORSet = #orset{removes = Removes}) ->
    CurrentExisting = [Elem || Elem = {E1, _} <- raw_value(ORSet), E1 =:= E],
    Removes1 = lists:foldl(fun(R, Rs) ->
                                  sgset:add(R, Rs)
                          end, Removes, CurrentExisting),
    ORSet#orset{removes = Removes1}.

merge(#orset{adds = Adds0,
             removes = Removes0},
      #orset{adds = Adds1,
             removes = Removes1}) ->
    #orset{adds = sgset:merge(Adds0, Adds1),
           removes = sgset:merge(Removes0, Removes1)}.

raw_value(#orset{adds = Adds,
                 removes = Removes}) ->
    ordsets:subtract(sgset:value(Adds), sgset:value(Removes)).
value(ORSet) ->
    ordsets:from_list([E || {E, _} <- raw_value(ORSet)]).

id() ->
    {now(), node()}.

gc(ORSet) ->
    #orset{adds = raw_value(ORSet),
           removes = sgset:new()}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, add, E, C1, C2, Check) ->
    ID = id(),
    {add(ID, E, C1), C2, add(ID, E, Check)};
op(b, add, E, C1, C2, Check) ->
    ID = id(),
    {C1, add(ID, E, C2), add(ID, E, Check)};
op(ab, add, E, C1, C2, Check) ->
    ID = id(),
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
