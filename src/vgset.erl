-module(vgset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, add/2, merge/2, value/1, from_list/1]).

-record(vgset, {payload = []}).

-opaque vgset() :: #vgset{}.

-export_type([vgset/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty Vgset.
%% @end
%%--------------------------------------------------------------------

-spec new() -> vgset().

new() ->
    #vgset{}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new  Vgset with the elements from the provided list.
%% @end
%%--------------------------------------------------------------------

from_list(L) ->
    #vgset{payload = ordsets:from_list(L)}.
%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the Vgset.
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), Vgset::vgset()) -> Vgset1::vgset().

add(Element, #vgset{payload = Payload}) ->
    #vgset{payload = ordsets:add_element(Element, Payload)}.

%%--------------------------------------------------------------------
%% @doc
%% Merges tow Vgsets.
%% @end
%%--------------------------------------------------------------------
-spec merge(Vgset1::vgset(), Vgset2::vgset()) -> Vgset::vgset().

merge(#vgset{payload = Payload1}, #vgset{payload = Payload2}) ->
    #vgset{payload = ordsets:union(Payload1, Payload2)}.


%%--------------------------------------------------------------------
%% @doc
%% Gets the value of a Vgset.
%% @end
%%--------------------------------------------------------------------
-spec value(Vgset::vgset()) -> [Element::term()].

value(#vgset{payload = Payload}) ->
    Payload.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

%% Apply a option either to the first set the second one or both.
%% Also keeps track of all changes in a check set (3rd).
op(a, E, Vgset1, Vgset2, Check) ->
    {add(E, Vgset1), Vgset2, add(E, Check)};

op(b, E, Vgset1, Vgset2, Check) ->
    {Vgset1, add(E, Vgset2), add(E, Check)};

op(ab, E, Vgset1, Vgset2, Check) ->
    {add(E, Vgset1), add(E, Vgset2), add(E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, E}, {A, B, C}) ->
                        op(T, E, A, B, C)
                end, {new(), new(), new()}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), integer()}).

prop_vgset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
