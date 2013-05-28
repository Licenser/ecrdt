-module(vlwwregister).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, assign/2, merge/2, value/1]).

-record(vlwwregister, {value, t}).

-opaque vlwwregister() :: #vlwwregister{}.

-export_type([vlwwregister/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

new(V) ->
    #vlwwregister{t = now(), value = V}.

assign(V, _) ->
    #vlwwregister{value = V, t = now()}.

merge(R1 = #vlwwregister{t = T0},
      #vlwwregister{t = T1}) when T0 > T1->
    R1;

merge(_, R2) ->
    R2.

value(#vlwwregister{value = V}) ->
    V.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, E, C1, C2, Check) ->
    {assign(E, C1), C2, assign(E, Check)};

op(b, E, C1, C2, Check) ->
    {C1, assign(E, C2), assign(E, Check)};

op(ab, E, C1, C2, Check) ->
    {assign(E, C1), assign(E, C2), assign(E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    Init = new(0),
    lists:foldl(fun({T, E}, {A, B, C}) ->
                        op(T, E, A, B, C)
                end, {Init, Init, Init}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]),  pos_integer()}).

prop_mmcounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:=value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).
-endif.
