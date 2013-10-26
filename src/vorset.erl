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

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([is_a/1, type/0, new/0, add/2, add/3, remove/2, merge/2, value/1,
         from_list/1, gc/1]).

-record(vorset, {adds :: vgset:vgset(),
                 removes :: vgset:vgset()}).

-opaque vorset() :: #vorset{}.

-export_type([vorset/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tests is the passed data is implementing this type.
%% @end
%%--------------------------------------------------------------------
-spec is_a(any()) -> true | false.

is_a(#vorset{}) ->
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
    set.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty OR Set.
%% @end
%%--------------------------------------------------------------------
-spec new() -> vorset().
new() ->
    #vorset{adds = vgset:new(),
           removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new OR Set form a list by adding each element in the
%% list.
%% @end
%%--------------------------------------------------------------------
-spec from_list(list()) -> vorset().
from_list(L) ->
    ID = ecrdt:id(),
    #vorset{adds = vgset:from_list([ {E, ID} || E <- L]),
           removes = vgset:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the OR set with a given master ID.
%% @end
%%--------------------------------------------------------------------
-spec add(ID::term(), Element::term(), ORSet::vorset()) -> ORSet1::vorset().
add(ID, Element, ORSet = #vorset{adds = Adds}) ->
    ORSet#vorset{adds = vgset:add({Element, ID}, Adds)}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the OR set using the default ID function
%% provided by ecrdt:id().
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), ORSet::vorset()) -> ORSet1::vorset().
add(Element, ORSet = #vorset{adds = Adds}) ->
    ORSet#vorset{adds = vgset:add({Element, ecrdt:id()}, Adds)}.

%%--------------------------------------------------------------------
%% @doc
%% Removes a element from the OR set by finding all observed adds and
%% putting them in the list of removed items.
%% @end
%%--------------------------------------------------------------------
-spec remove(Element::term(), ORSet::vorset()) -> ORSet1::vorset().
remove(Element, ORSet = #vorset{removes = Removes}) ->
    CurrentExisting = [Elem || Elem = {E1, _} <- raw_value(ORSet), E1 =:= Element],
    Removes1 = lists:foldl(fun(R, Rs) ->
                                  vgset:add(R, Rs)
                          end, Removes, CurrentExisting),
    ORSet#vorset{removes = Removes1}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two OR Sets by taking the union of adds and removes.
%% @end
%%--------------------------------------------------------------------
-spec merge(ORSet0::vorset(), ORSet1::vorset()) -> ORSetM::vorset().
merge(#vorset{adds = Adds0,
             removes = Removes0},
      #vorset{adds = Adds1,
             removes = Removes1}) ->
    #vorset{adds = vgset:merge(Adds0, Adds1),
           removes = vgset:merge(Removes0, Removes1)}.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an OR Set by taking the
%% substract of adds and removes then eleminating the ID field.
%% @end
%%--------------------------------------------------------------------
-spec value(ORSet::vorset()) -> [Element::term()].
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
-spec gc(ORSet::vorset()) -> ORSetGCed::vorset().
gc(ORSet) ->
    #vorset{adds = vgset:from_list(raw_value(ORSet)),
           removes = vgset:new()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_value(ORSet::vorset()) -> ordsets:ordset().
raw_value(#vorset{adds = Adds,
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

prop_vorset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([],
                 proper:module(
                   ?MODULE,
                   [{numtests, 1000},
                    {to_file, user},
                    long_result])).

-endif.
