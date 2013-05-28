-module(lwwregister).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, assign/2, downstream/2, merge/2, value/1, gc/1]).

-record(lwwregister, {value, t}).

-opaque lwwregister() :: #lwwregister{}.

-export_type([lwwregister/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

new(V) ->
    #lwwregister{t = now(), value = V}.

assign(V, _) ->
    T = now(),
    M = {T, V},
    {M, #lwwregister{value = V,
                     t = T}}.

downstream({T, V}, #lwwregister{t = T0}) when T > T0 ->
    #lwwregister{value = V, t = T};

downstream(_, R) ->
    R.

merge(R1 = #lwwregister{t = T0},
      #lwwregister{t = T1}) when T0 > T1->
    R1;
merge(_, R2) ->
    R2.



gc(R) ->
    R.

value(#lwwregister{value = V}) ->
    V.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, E, C1, C2, Check) ->
    {M, C11} = assign(E, C1),
    Check1 = downstream(M, Check),
    {C11, C2, Check1};

op(b, E, C1, C2, Check) ->
    {M, C21} = assign(E, C2),
    Check1 = downstream(M, Check),
    {C1, C21, Check1};

op(ab, E, C1, C2, Check) ->
    {M, C11} = assign(E, C1),
    C21 = downstream(M, C2),
    Check1 = downstream(M, Check),
    {C11, C21, Check1}.

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
