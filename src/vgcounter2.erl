-module(vgcounter2).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, value/1, inc/3, merge/2]).

-record(vgcounter2, {vector = []}).

new() ->
    #vgcounter2{}.


inc(Master, Increment,
    #vgcounter2{vector = Vector0}) when Increment > 0 ->
    case lists:keytake(Master, 1, Vector0) of
        false ->
            #vgcounter2{vector = [{Master, Increment} | Vector0]};
        {value, {_, V0}, Vector1} ->
            #vgcounter2{vector = [{Master, V0 + Increment} | Vector1]}
    end.

merge(#vgcounter2{vector = Vector0},
      #vgcounter2{vector = Vector1}) ->
    #vgcounter2{vector = merge_vectors(lists:sort(Vector0), lists:sort(Vector1), [])}.


%% If the master exists in both vectors take the bigger value
merge_vectors([{M, V0} | R0],
              [{M, _V1} | R1],
              R) when V0 >= _V1->
    merge_vectors(R0, R1, [{M, V0} | R]);
merge_vectors([{M, _} | R0],
              [{M, V1} | R1],
              R) ->
    merge_vectors(R0, R1, [{M, V1} | R]);

%% If the master on V0 is bigger add.
merge_vectors([{_M0, _} = E0 | R0],
              [{_M1, _} | _] = R1,
              R) when _M0 < _M1 ->
    merge_vectors(R0, R1, [E0 | R]);
%% If the master on V1 is bigger add it.
merge_vectors([{_M0, _} | _] = R0,
              [{_M1, _} = E1 |  R1 ],
              R) when _M0 > _M1 ->
    merge_vectors(R0, R1, [E1 | R]);

merge_vectors(R0, [], R) ->
    R0 ++ R;

merge_vectors([], R1, R) ->
    R1 ++ R.

value(#vgcounter2{vector = Vector}) ->
    lists:sum([V || {_, V} <- Vector]).

-ifdef(TEST).

op(a, E, C1, C2, Check) ->
    {inc(a, E, C1), C2, inc(a, E, Check)};

op(b, E, C1, C2, Check) ->
    {C1, inc(b, E, C2), inc(b, E, Check)};

op(ab1, E, C1, C2, Check) ->
    {inc(a, E, C1), inc(a, E, C2), inc(a, E, Check)};

op(ab2, E, C1, C2, Check) ->
    {inc(b, E, C1), inc(b, E, C2), inc(b, E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    O = new(),
    lists:foldl(fun({T, E}, {A, B, C}) ->
                        op(T, E, A, B, C)
                end, {O, O, O}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab1, ab2]), pos_integer()}).

prop_vgcounter2() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
