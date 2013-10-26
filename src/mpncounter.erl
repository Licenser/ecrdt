-module(mpncounter).

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([type/0, is_a/1, new/0, inc/2, dec/2, downstream/2, replay/2, value/1,
         gc/1]).

-record(mpncounter, {value = 0, messages = []}).

-opaque mpncounter() :: #mpncounter{}.

-export_type([mpncounter/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tests is the passed data is implementing this type.
%% @end
%%--------------------------------------------------------------------
-spec is_a(any()) -> true | false.

is_a(#mpncounter{}) ->
    true;

is_a(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Returns the type of this object
%% @end
%%--------------------------------------------------------------------
-spec type() -> register | set | gset | counter | gcounter | map.

type() ->
    counter.

new() ->
    #mpncounter{}.

inc(Inc, #mpncounter{value = V, messages = Msgs}) ->
    ID = ecrdt:id(),
    M = {ID, inc, Inc},
    {M, #mpncounter{value = V + Inc,
                   messages = ordsets:add_element(ID, Msgs)}}.

dec(Dec, #mpncounter{value = V, messages = Msgs}) ->
    ID = ecrdt:id(),
    M = {ID, dec, Dec},
    {M, #mpncounter{value = V - Dec,
                   messages = ordsets:add_element(ID, Msgs)}}.

downstream({ID, inc, Inc}, C = #mpncounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(ID, Msgs) of
        true ->
            C;
        _ ->
            C#mpncounter{value = V + Inc,
                        messages = ordsets:add_element(ID, Msgs)}
    end;

downstream({ID, dec, Dec}, C = #mpncounter{value = V, messages = Msgs}) ->
    case ordsets:is_element(ID, Msgs) of
        true ->
            C;
        _ ->
            C#mpncounter{value = V - Dec,
                        messages = ordsets:add_element(ID, Msgs)}
    end.

replay(Messages, Counter) ->
    lists:foldl(fun(M, C) ->
                        downstream(M, C)
                end, Counter, Messages).

gc(#mpncounter{value = V}) ->
    #mpncounter{value = V}.

value(#mpncounter{value = V}) ->
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
