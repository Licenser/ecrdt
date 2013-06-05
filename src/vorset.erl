%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% An Implementation of the OR Set (Observe Remove Set) CvRDT. It
%%% improves over the 2P set by allowing to readd removed elemetns at
%%% the cost of keeping a tumbstone set and requiring master
%%% identification.
%%% @end
%%% Created :  1 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(vorset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, add/2, add/3, remove/2, merge/2, value/1, from_list/1, gc/1]).

-record(orset, {adds :: vgset:vgset(),
                removes :: vgset:vgset()}).

-opaque orset() :: #orset{}.

-export_type([orset/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty OR Set.
%% @end
%%--------------------------------------------------------------------
-spec new() -> orset().
new() ->
    #orset{adds = vgset:new(),
           removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new OR Set form a list by adding each element in the
%% list.
%% @end
%%--------------------------------------------------------------------
-spec from_list(list()) -> orset().
from_list(L) ->
    ID = ecrdt:id(),
    #orset{adds = vgset:from_list([ {E, ID} || E <- L]),
           removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the OR set with a given master ID.
%% @end
%%--------------------------------------------------------------------
-spec add(ID::term(), Element::term(), ORSet::orset()) -> ORSet1::orset().
add(ID, Element, ORSet = #orset{adds = Adds}) ->
    ORSet#orset{adds = vgset:add({Element, ID}, Adds)}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the OR set using the default ID function
%% provided by ecrdt:id().
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), ORSet::orset()) -> ORSet1::orset().
add(Element, ORSet = #orset{adds = Adds}) ->
    ORSet#orset{adds = vgset:add({Element, ecrdt:id()}, Adds)}.

%%--------------------------------------------------------------------
%% @doc
%% Removes a element from the OR set by finding all observed adds and
%% putting them in the list of removed items.
%% @end
%%--------------------------------------------------------------------
-spec remove(Element::term(), ORSet::orset()) -> ORSet1::orset().
remove(Element, ORSet = #orset{removes = Removes}) ->
    CurrentExisting = [Elem || Elem = {E1, _} <- raw_value(ORSet), E1 =:= Element],
    Removes1 = lists:foldl(fun(R, Rs) ->
                                  vgset:add(R, Rs)
                          end, Removes, CurrentExisting),
    ORSet#orset{removes = Removes1}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two OR Sets by taking the union of adds and removes.
%% @end
%%--------------------------------------------------------------------
-spec merge(ORSet0::orset(), ORSet1::orset()) -> ORSetM::orset().
merge(#orset{adds = Adds0,
             removes = Removes0},
      #orset{adds = Adds1,
             removes = Removes1}) ->
    #orset{adds = vgset:merge(Adds0, Adds1),
           removes = vgset:merge(Removes0, Removes1)}.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an OR Set by taking the
%% substract of adds and removes then eleminating the ID field.
%% @end
%%--------------------------------------------------------------------
-spec value(ORSet::orset()) -> [Element::term()].
value(ORSet) ->
    ordsets:from_list([E || {E, _} <- raw_value(ORSet)]).

%%--------------------------------------------------------------------
%% @doc
%% Garbage collects a OR set by storing the currently existing values
%% only as adds and clearing the removes.
%%
%% Be aware that this needs to be carried out syncronously or will
%% lead to unexpected results!
%% @end
%%--------------------------------------------------------------------
-spec gc(ORSet::orset()) -> ORSetGCed::orset().
gc(ORSet) ->
    #orset{adds = raw_value(ORSet),
           removes = vgset:new()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_value(ORSet::orset()) -> [{Element::term(), ID::term()}].
raw_value(#orset{adds = Adds,
                 removes = Removes}) ->
    ordsets:subtract(vgset:value(Adds), vgset:value(Removes)).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, add, E, C1, C2, Check) ->
    ID = ecrdt:id(),
    {add(ID, E, C1), C2, add(ID, E, Check)};
op(b, add, E, C1, C2, Check) ->
    ID = ecrdt:id(),
    {C1, add(ID, E, C2), add(ID, E, Check)};
op(ab, add, E, C1, C2, Check) ->
    ID = ecrdt:id(),
    {add(ID, E, C1), add(ID, E, C2), add(ID, E, Check)};
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

prop_orset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
