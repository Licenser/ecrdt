-module(ocounter).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, inc/2, dec/2, downstream/2, merge/2, value/1, gc/1]).

-record(orcounter, {value = 0, messages = []}).

-opaque orcounter() :: #orcounter{}.

-export_type([orcounter/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

new() ->
    #orcounter{}.

inc(Inc, #orcounter{value = V, messages = Msgs}) ->
    M = {ecrdt:id(), inc, Inc},
    {M, #orcounter{value = V + Inc,
                   messages = ordsets:add_element(M, Msgs)}}.

dec(Dec, #orcounter{value = V, messages = Msgs}) ->
    M = {ecrdt:id(), dec, Dec},
    {M, #orcounter{value = V - Dec,
                   messages = ordsets:add_element(M, Msgs)}}.

downstream(M = {_, inc, Inc}, C = #orcounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(M, Msgs) of
        true ->
            C;
        _ ->
            C#orcounter{value = V + Inc,
                        messages = ordsets:add_element(M, Msgs)}
    end;

downstream(M = {_, dec, Dec}, C = #orcounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(M, Msgs) of
        true ->
            C;
        _ ->
            C#orcounter{value = V - Dec,
                        messages = ordsets:add_element(M, Msgs)}
    end.


merge(#orcounter{value = V1, messages = Msgs1},
      #orcounter{messages = Msgs2}) ->
    New = ordsets:subtract(Msgs2, Msgs1),
    VR = lists:foldl(fun ({_, inc, Inc}, VAcc) ->
                            VAcc + Inc;
                        ({_, dec, Dec}, VAcc) ->
                            VAcc - Dec
                    end, V1, New),
    #orcounter{value = VR, messages = ordsets:union(Msgs1, New)}.

gc(#orcounter{value = V}) ->
    #orcounter{value = V}.

value(#orcounter{value = V}) ->
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
