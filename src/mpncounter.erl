-module(mpncounter).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, inc/2, dec/2, downstream/2, merge/2, value/1, gc/1]).

-record(moncounter, {value = 0, messages = []}).

-opaque moncounter() :: #moncounter{}.

-export_type([moncounter/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

new() ->
    #moncounter{}.

inc(Inc, #moncounter{value = V, messages = Msgs}) ->
    M = {ecrdt:id(), inc, Inc},
    {M, #moncounter{value = V + Inc,
                   messages = ordsets:add_element(M, Msgs)}}.

dec(Dec, #moncounter{value = V, messages = Msgs}) ->
    M = {ecrdt:id(), dec, Dec},
    {M, #moncounter{value = V - Dec,
                   messages = ordsets:add_element(M, Msgs)}}.

downstream(M = {_, inc, Inc}, C = #moncounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(M, Msgs) of
        true ->
            C;
        _ ->
            C#moncounter{value = V + Inc,
                        messages = ordsets:add_element(M, Msgs)}
    end;

downstream(M = {_, dec, Dec}, C = #moncounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(M, Msgs) of
        true ->
            C;
        _ ->
            C#moncounter{value = V - Dec,
                        messages = ordsets:add_element(M, Msgs)}
    end.


merge(#moncounter{value = V1, messages = Msgs1},
      #moncounter{messages = Msgs2}) ->
    New = ordsets:subtract(Msgs2, Msgs1),
    VR = lists:foldl(fun ({_, inc, Inc}, VAcc) ->
                            VAcc + Inc;
                        ({_, dec, Dec}, VAcc) ->
                            VAcc - Dec
                    end, V1, New),
    #moncounter{value = VR, messages = ordsets:union(Msgs1, New)}.

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

op(a, inc, E, C1, C2, Check) ->
    {M, C11} = inc(E, C1),
    Check1 = downstream(M, Check),
    {C11, C2, Check1};

op(b, inc, E, C1, C2, Check) ->
    {M, C21} = inc(E, C2),
    Check1 = downstream(M, Check),
    {C1, C21, Check1};

op(ab, inc, E, C1, C2, Check) ->
    {M, C11} = inc(E, C1),
    C21 = downstream(M, C2),
    Check1 = downstream(M, Check),
    {C11, C21, Check1};

op(a, dec, E, C1, C2, Check) ->
    {M, C11} = dec(E, C1),
    Check1 = downstream(M, Check),
    {C11, C2, Check1};

op(b, dec, E, C1, C2, Check) ->
    {M, C21} = dec(E, C2),
    Check1 = downstream(M, Check),
    {C1, C21, Check1};

op(ab, dec, E, C1, C2, Check) ->
    {M, C11} = dec(E, C1),
    C21 = downstream(M, C2),
    Check1 = downstream(M, Check),
    {C11, C21, Check1}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, E}, {A, B, C}) ->
                        op(T, O, E, A, B, C)
                end, {new(), new(), new()}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), oneof([inc, dec]),  pos_integer()}).

prop_mmcounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:=value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).
-endif.
