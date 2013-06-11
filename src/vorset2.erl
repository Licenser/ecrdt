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

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, add/2, add/3, remove/2, merge/2, value/1, from_list/1, gc/1]).

-record(orset2, {values :: [{Element::term(), ID::term()}],
                 seen :: [ID::term()]}).

-opaque orset2() :: #orset2{}.

-export_type([orset2/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty OR Set.
%% @end
%%--------------------------------------------------------------------
-spec new() -> orset2().
new() ->
    #orset2{values = [],
            seen = []}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new OR Set form a list by adding each element in the
%% list.
%% @end
%%--------------------------------------------------------------------
-spec from_list(list()) -> orset2().
from_list(L) ->
    Values = [ {E, ecrdt:id()} || E <- L],
    #orset2{values = Values,
            seen = ordsets:from_list([ID || {_, ID} <- Values])}.

%%--------------------------------------------------------------------
%% @doc
%% Values an element to the OR set using the default ID function
%% provided by ecrdt:id().
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), ORSet::orset2()) -> ORSet1::orset2().
add(Element, ORSet) ->
    add(ecrdt:id(), Element, ORSet).

%%--------------------------------------------------------------------
%% @doc
%% Values an element to the OR set with a given master ID.
%% @end
%%--------------------------------------------------------------------
-spec add(ID::term(), Element::term(), ORSet::orset2()) -> ORSet1::orset2().
add(ID, Element, ORSet = #orset2{values = Values, seen = Seen}) ->
    case lists:member(ID, Seen) of
        true ->
            ORSet;
        false ->
            ORSet#orset2{values = [{Element, ID} | Values],
                         seen = ordsets:add_element(ID, Seen)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes a element from the OR set.
%% @end
%%--------------------------------------------------------------------
-spec remove(Element::term(), ORSet::orset2()) -> ORSet1::orset2().
remove(Element, ORSet = #orset2{values = Values}) ->
    ORSet#orset2{values = [E || {_V, _} = E <- Values, _V =/= Element]}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two OR Sets.
%% @end
%%--------------------------------------------------------------------
-spec merge(ORSet0::orset2(), ORSet1::orset2()) -> ORSetM::orset2().
merge(ORSet0,
      #orset2{values = Values, seen = Seen}) ->
    merge_sets(ORSet0, Values, Seen).

merge_sets(ORSet, [], []) ->
    ORSet;
merge_sets(ORSet = #orset2{values = Values,
                           seen = Seen}, [], [ID | R]) ->
    merge_sets(ORSet#orset2{values = lists:keydelete(ID, 2, Values),
                            seen = ordsets:add_element(ID, Seen)}, [], R);
merge_sets(ORSet, [{Element, ID} | R], Seen) ->
    merge_sets(add(ID, Element, ORSet), R, lists:delete(ID, Seen)).

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an OR Set.
%% @end
%%--------------------------------------------------------------------
-spec value(ORSet::orset2()) -> [Element::term()].
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
-spec gc(ORSet::orset2()) -> ORSetGCed::orset2().
gc(#orset2{values = Values}) ->
    #orset2{values = Values,
            seen = [ID || {_, ID} <-Values]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_value(ORSet::orset2()) -> [{Element::term(), ID::term()}].
raw_value(#orset2{values = Values}) ->
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
