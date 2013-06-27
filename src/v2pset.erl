%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% An implementation of a 2P sets, a add remove set in which elemetns
%%% can be added, removed but not readded again.
%%% @end
%%% Created :  1 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(v2pset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, add/2, remove/2, merge/2, value/1, from_list/1, gc/1]).

-record(v2pset, {adds :: vgset:vgset(),
                 removes :: vgset:vgset()}).

-opaque v2pset() :: #v2pset{}.

-export_type([v2pset/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty 2P set.
%% @end
%%--------------------------------------------------------------------
-spec new() -> v2pset().
new() ->
    #v2pset{adds = vgset:new(),
            removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a 2p set from an existing list by adding all it's elements.
%% @end
%%--------------------------------------------------------------------
-spec from_list(list()) -> v2pset().
from_list(L) ->
    #v2pset{adds = vgset:from_list(L),
            removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the 2p set, if this element was removed before
%% this function has no efect.
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), V2pset::v2pset()) -> V2pset1::v2pset().
add(Element, V2pset = #v2pset{adds = Adds}) ->
    V2pset#v2pset{adds = vgset:add(Element, Adds)}.

%%--------------------------------------------------------------------
%% @doc
%% removes an element to the 2p set, this call is final it can never
%% be added again.
%% @end
%%--------------------------------------------------------------------
-spec remove(Element::term(), V2pset::v2pset()) -> V2pset1::v2pset().
remove(Element, V2pset = #v2pset{removes = Removes}) ->
    V2pset#v2pset{removes = vgset:add(Element, Removes)}.

%%--------------------------------------------------------------------
%% @doc
%% Gets teh value of the 2p set by taking substracting te removed
%% elements from the added ones.
%% @end
%%--------------------------------------------------------------------
-spec value(V2pset::v2pset()) -> [Element::term()].
value(#v2pset{adds = Adds,
              removes = Removes}) ->
    ordsets:subtract(vgset:value(Adds), vgset:value(Removes)).

%%--------------------------------------------------------------------
%% @doc
%% Merges two 2p sets by creating the union of adds and removes.
%% @end
%%--------------------------------------------------------------------
-spec merge(V2pset0::v2pset(), V2pset1::v2pset()) -> V2psetM::v2pset().
merge(#v2pset{adds = Adds0,
              removes = Removes0},
      #v2pset{adds = Adds1,
              removes = Removes1}) ->
    #v2pset{adds = vgset:merge(Adds0, Adds1),
            removes = vgset:merge(Removes0, Removes1)}.

%%--------------------------------------------------------------------
%% @doc
%% Garbace collects the 2p set by computing the value, storing it as
%% adds and emptying the removes.
%%
%% Be careful this will require syncronisation between all copies
%% it the result is not going to be predictable!
%% @end
%%--------------------------------------------------------------------
-spec gc(V2pset::v2pset()) -> V2psetGCed::v2pset().
gc(V2pset) ->
    #v2pset{adds = vgset:from_list(value(V2pset)),
            removes = vgset:new()}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, add, E, C1, C2, Check) ->
    {add(E, C1), C2, add(E, Check)};
op(b, add, E, C1, C2, Check) ->
    {C1, add(E, C2), add(E, Check)};
op(ab, add, E, C1, C2, Check) ->
    {add(E, C1), add(E, C2), add(E, Check)};
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

prop_v2pset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
