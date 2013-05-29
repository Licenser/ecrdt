-module(mpncounter).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, inc/2, dec/2, downstream/2, replay/2, value/1, gc/1]).

-record(moncounter, {value = 0, messages = []}).

-opaque moncounter() :: #moncounter{}.

-export_type([moncounter/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

new() ->
    #moncounter{}.

inc(Inc, #moncounter{value = V, messages = Msgs}) ->
    ID = ecrdt:id(),
    M = {ID, inc, Inc},
    {M, #moncounter{value = V + Inc,
                   messages = ordsets:add_element(ID, Msgs)}}.

dec(Dec, #moncounter{value = V, messages = Msgs}) ->
    ID = ecrdt:id(),
    M = {ID, dec, Dec},
    {M, #moncounter{value = V - Dec,
                   messages = ordsets:add_element(ID, Msgs)}}.

downstream({ID, inc, Inc}, C = #moncounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(ID, Msgs) of
        true ->
            C;
        _ ->
            C#moncounter{value = V + Inc,
                        messages = ordsets:add_element(ID, Msgs)}
    end;

downstream({ID, dec, Dec}, C = #moncounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(ID, Msgs) of
        true ->
            C;
        _ ->
            C#moncounter{value = V - Dec,
                        messages = ordsets:add_element(ID, Msgs)}
    end.

replay(Messages, Counter) ->
    lists:foldl(fun(M, C) ->
                        downstream(M, C)
                end, Counter, Messages).

gc(#moncounter{value = V}) ->
    #moncounter{value = V}.

value(#moncounter{value = V}) ->
    V.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, inc, E, C1, M1, C2, M2, Check) ->
    {M, C11} = inc(E, C1),
    Check1 = downstream(M, Check),
    {C11, M1, C2, [M|M2], Check1};

op(b, inc, E, C1, M1, C2, M2, Check) ->
    {M, C21} = inc(E, C2),
    Check1 = downstream(M, Check),
    {C1, [M|M1], C21, M2, Check1};

op(ab, inc, E, C1, M1, C2, M2, Check) ->
    {M, C11} = inc(E, C1),
    C21 = downstream(M, C2),
    Check1 = downstream(M, Check),
    {C11, M1, C21, M2, Check1};

op(a, dec, E, C1, M1, C2, M2, Check) ->
    {M, C11} = dec(E, C1),
    Check1 = downstream(M, Check),
    {C11, M1, C2, [M|M2], Check1};

op(b, dec, E, C1, M1, C2, M2, Check) ->
    {M, C21} = dec(E, C2),
    Check1 = downstream(M, Check),
    {C1, [M|M1], C21, M2, Check1};

op(ab, dec, E, C1, M1, C2, M2, Check) ->
    {M, C11} = dec(E, C1),
    C21 = downstream(M, C2),
    Check1 = downstream(M, Check),
    {C11, M1, C21, M2, Check1}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, E}, {A, MA, B, MB, C}) ->
                        op(T, O, E, A, MA, B, MB, C)
                end, {new(), [], new(), [], new()}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), oneof([inc, dec]),  pos_integer()}).

prop_mmcounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, Ma, B, Mb, C} = apply_ops(Ts),
                value(C) =:= value(replay(Ma, A)) andalso
                    value(C) =:= value(replay(Mb, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).
-endif.
