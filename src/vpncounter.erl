-module(vpncounter).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, value/1, inc/3, merge/2]).

-record(mmicounter, {vector, size}).

new(Size) ->
    L = [ 0 || _ <- lists:seq(1, Size)],
    #mmicounter{size = Size, vector = list_to_tuple(L)}.


inc(Master, Increment,
    Counter = #mmicounter{vector = Vector0,
                          size = _Size}) when Increment > 0,
                                              Master =< _Size ->
    Value0 = element(Master, Vector0),
    Value1 = Value0 + Increment,
    Vector1 = setelement(Master, Vector0, Value1),
    Counter#mmicounter{vector = Vector1}.

merge(Counter0 = #mmicounter{vector = Vector0, size = _Size},
      #mmicounter{vector = Vector1, size = _Size}) ->
    VectorM = list_to_tuple(
                merge_vecotrs(tuple_to_list(Vector0),
                              tuple_to_list(Vector1),
                              [])),
    Counter0#mmicounter{vector = VectorM}.

value(#mmicounter{vector = Vector0}) ->
    Vector1 = tuple_to_list(Vector0),
    lists:foldl(fun(V, Acc) ->
                        Acc + V
                end, 0, Vector1).

merge_vecotrs([V0 | R0], [_V1 | R1], Acc) when V0 >= _V1 ->
    merge_vecotrs(R0, R1, [V0 | Acc]);
merge_vecotrs([_ | R0], [V1 | R1], Acc) ->
    merge_vecotrs(R0, R1, [V1 | Acc]);
merge_vecotrs([], [], Acc) ->
    lists:reverse(Acc).


-ifdef(TEST).

op(a, E, C1, C2, Check) ->
    {inc(1, E, C1), C2, inc(1, E, Check)};

op(b, E, C1, C2, Check) ->
    {C1, inc(2, E, C2), inc(2, E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, E}, {A, B, C}) ->
                        op(T, E, A, B, C)
                end, {new(2), new(2), new(2)}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b]), pos_integer()}).

prop_mmicounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
