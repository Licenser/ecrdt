%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% An Implementation of the OR Set (Observe Remove Set) CvRDT. It
%%% is optimized over the default implementation. Details about the
%%% optimization are based on the paper:
%%% http://www.cmi.ac.in/~madhavan/papers/mss-tr-2013.html
%%% @end
%%% Created :  1 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(vorset2).

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([type/0, is_a/1, new/0, add/2, add/3, remove/2, merge/2, value/1,
         from_list/1, gc/1]).

-record(vorset2, {values :: [{Element::term(), ID::term()}],
                  seen :: [ID::term()]}).

-opaque vorset2() :: #vorset2{}.

-export_type([vorset2/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tests is the passed data is implementing this type.
%% @end
%%--------------------------------------------------------------------
-spec is_a(any()) -> true | false.

is_a(#vorset2{}) ->
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
-spec new() -> vorset2().
new() ->
    #vorset2{values = [],
            seen = []}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new OR Set form a list by adding each element in the
%% list.
%% @end
%%--------------------------------------------------------------------
-spec from_list(list()) -> vorset2().
from_list(L) ->
    Values = [ {E, ecrdt:id()} || E <- L],
    #vorset2{values = Values,
            seen = ordsets:from_list([ID || {_, ID} <- Values])}.

%%--------------------------------------------------------------------
%% @doc
%% Values an element to the OR set using the default ID function
%% provided by ecrdt:id().
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), ORSet::vorset2()) -> ORSet1::vorset2().
add(Element, ORSet) ->
    add(ecrdt:id(), Element, ORSet).

%%--------------------------------------------------------------------
%% @doc
%% Values an element to the OR set with a given master ID.
%% @end
%%--------------------------------------------------------------------
-spec add(ID::term(), Element::term(), ORSet::vorset2()) -> ORSet1::vorset2().
add(ID, Element, ORSet = #vorset2{values = Values, seen = Seen}) ->
    case lists:member(ID, Seen) of
        true ->
            ORSet;
        false ->
            ORSet#vorset2{values = [{Element, ID} | Values],
                         seen = ordsets:add_element(ID, Seen)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes a element from the OR set.
%% @end
%%--------------------------------------------------------------------
-spec remove(Element::term(), ORSet::vorset2()) -> ORSet1::vorset2().
remove(Element, ORSet = #vorset2{values = Values}) ->
    ORSet#vorset2{values = [E || {_V, _} = E <- Values, _V =/= Element]}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two OR Sets.
%% @end
%%--------------------------------------------------------------------
-spec merge(ORSet0::vorset2(), ORSet1::vorset2()) -> ORSetM::vorset2().
merge(ORSet0,
      #vorset2{values = Values, seen = Seen}) ->
    merge_sets(ORSet0, Values, Seen).

merge_sets(ORSet, [], []) ->
    ORSet;
merge_sets(ORSet = #vorset2{values = Values,
                           seen = Seen}, [], [ID | R]) ->
    merge_sets(ORSet#vorset2{values = lists:keydelete(ID, 2, Values),
                            seen = ordsets:add_element(ID, Seen)}, [], R);
merge_sets(ORSet, [{Element, ID} | R], Seen) ->
    merge_sets(add(ID, Element, ORSet), R, lists:delete(ID, Seen)).

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an OR Set.
%% @end
%%--------------------------------------------------------------------
-spec value(ORSet::vorset2()) -> [Element::term()].
value(ORSet) ->
    ordsets:from_list([E || {E, _} <- raw_value(ORSet)]).

%%--------------------------------------------------------------------
%% @doc
%% Garbage collects a OR set by removing all delted IDs.
%%
%% Be aware that this needs to be carried out syncronously or will
%% lead to unexpected results!
%% @end
%%--------------------------------------------------------------------
-spec gc(ORSet::vorset2()) -> ORSetGCed::vorset2().
gc(#vorset2{values = Values}) ->
    #vorset2{values = Values,
            seen = [ID || {_, ID} <-Values]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_value(ORSet::vorset2()) -> [{Element::term(), ID::term()}].
raw_value(#vorset2{values = Values}) ->
    Values.

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
    case lists:member(E, value(C1)) of
        true ->
            {remove(E, C1), C2, remove(E, Check)};
        false ->
            {C1, C2, Check}
    end;
op(b, remove, E, C1, C2, Check) ->
    case lists:member(E, value(C2)) of
        true ->
            {C1, remove(E, C2), remove(E, Check)};
        false ->
            {C1, C2, Check}
    end;
op(ab, remove, E, C1, C2, Check) ->
    case {lists:member(E, value(C1)), lists:member(E, value(C2))} of
        {true, true} ->
            {remove(E, C1), remove(E, C2), remove(E, Check)};
        _ ->
            {C1, C2, Check}
    end.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, E}, {A, B, C}) ->
                        op(T, O, E, A, B, C)
                end, {new(), new(), new()}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), oneof([add, remove]), pos_integer()}).

prop_orset2() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C)) andalso
                    value(C) =:= value(merge(merge(B, A), C)) andalso
                    value(merge(merge(A, B), C)) =:= value(merge(merge(B, A), C))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE,
                                   [{to_file, user}, long_result, {numtests, 500}])).

-endif.
