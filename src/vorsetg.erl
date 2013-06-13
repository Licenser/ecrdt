%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% This is an extention of the standard OR Set implementation that
%%% adds the GC interface and functionality as descrived here:
%%% http://blog.licenser.net/blog/2013/06/11/asyncronous-garbage-collection-with-crdts/
%%% @end
%%% Created :  1 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(vorsetg).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         new/0, new/1,
         add/2, add/3,
         remove/2,
         merge/2,
         value/1,
         gc/2, gcable/1
        ]).

-record(vorsetg, {adds = [] :: ordsets:ordset(),
                  gced = [] :: ordsets:ordset(),
                  removes :: rot:rot()}).

-opaque vorsetg() :: #vorsetg{}.

-export_type([vorsetg/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty OR Set.
%% @end
%%--------------------------------------------------------------------
-spec new() -> vorsetg().
new() ->
    #vorsetg{removes = rot:new(now())}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty OR Set with a given bucket size for the GC rot.
%% @end
%%--------------------------------------------------------------------
-spec new(Size::pos_integer()) -> vorsetg().
new(Size) ->
    #vorsetg{removes = rot:new(now(), Size)}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the OR set with a given master ID.
%% @end
%%--------------------------------------------------------------------
-spec add(ID::term(), Element::term(), ORSet::vorsetg()) -> ORSet1::vorsetg().
add(ID, Element, ORSet = #vorsetg{adds = Adds}) ->
    ORSet#vorsetg{adds = ordsets:add_element({ID, Element}, Adds)}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the OR set using the default ID function
%% provided by ecrdt:id().
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), ORSet::vorsetg()) -> ORSet1::vorsetg().
add(Element, ORSet) ->
    add(ecrdt:id(), Element, ORSet).

%%--------------------------------------------------------------------
%% @doc
%% Removes a element from the OR set by finding all observed adds and
%% putting them in the list of removed items.
%% @end
%%--------------------------------------------------------------------
-spec remove(Element::term(), ORSet::vorsetg()) -> ORSet1::vorsetg().
remove(Element, ORSet = #vorsetg{removes = Removes}) ->
    CurrentExisting = [Elem || Elem = {_, E1} <- raw_value(ORSet),
                               E1 =:= Element],
    Now = os:timestamp(),
    Removes1 = lists:foldl(fun(R, Rs) ->
                                   rot:add({Now, R}, Rs)
                           end, Removes, CurrentExisting),
    ORSet#vorsetg{removes = Removes1}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two OR Sets by taking the union of adds and removes.
%% @end
%%--------------------------------------------------------------------
-spec merge(ORSet0::vorsetg(), ORSet1::vorsetg()) -> ORSetM::vorsetg().
merge(ROTA = #vorsetg{gced = GCedA},
      ROTB = #vorsetg{gced = GCedB}) ->
    #vorsetg{
       adds = AddsA,
       gced = GCed,
       removes = RemovesA}
        = lists:foldl(fun gc/2, ROTA, GCedB),
    #vorsetg{
       adds = AddsB,
       removes = RemovesB}
        = lists:foldl(fun gc/2, ROTB, GCedA),
    #vorsetg{adds = ordsets:union(AddsA, AddsB),
             gced = GCed,
             removes = rot:merge(RemovesA, RemovesB)}.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an OR Set by taking the
%% substract of adds and removes then eleminating the ID field.
%% @end
%%--------------------------------------------------------------------
-spec value(ORSet::vorsetg()) -> [Element::term()].
value(ORSet) ->
    ordsets:from_list([E || {_, E} <- raw_value(ORSet)]).

%%--------------------------------------------------------------------
%% @doc
%% Garbage collects a hash bucket from the OR set by removing it's
%% values from both the removes and adds.
%%
%% If the hash bucket was negotiated correctly this opperation can
%% be performed asyncronously without problems.
%% @end
%%--------------------------------------------------------------------
-spec gc(HashID::rot:hash(), ORSet::vorsetg()) -> ORSetGCed::vorsetg().
gc(HashID,
   #vorsetg{
      adds = Adds,
      removes = Removes,
      gced = GCed}) ->
    {Values, Removes1} = rot:remove(HashID, Removes),
    #vorsetg{adds = ordsets:subtract(Adds, Values),
             gced = ordsets:add_element(HashID, GCed),
             removes = Removes1}.

gcable(#vorsetg{removes = Removes}) ->
    rot:full(Removes).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec raw_value(ORSet::vorsetg()) -> [{Element::term(), ID::term()}].
raw_value(#vorsetg{adds = Adds,
                   removes = Removes}) ->
    ordsets:subtract(Adds, [E || {_, E} <- rot:value(Removes)]).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

gc_all(Obj, GCs) ->
    lists:foldl(fun gc/2, Obj, GCs).

gcable(A,B, Latest) ->
    GC = ordsets:union(gcable(A), gcable(B)),
    [G || G = {T, _} <- GC, T < Latest].

gcable(A, B, C, Latest) ->
    GC = ordsets:union([gcable(A), gcable(B), gcable(C)]),
    [G || G = {T, _} <- GC, T < Latest].


op(T, Op, E, C1, C2, Check, Now) ->
    op(?MODULE, T, Op, E, C1, C2, Check, Now).

op(Mod, a, add, E, C1, C2, Check, Now) ->
    ID = ecrdt:id(),
    {Mod:add(ID, E, C1), C2, Mod:add(ID, E, Check), Now};
op(Mod, b, add, E, C1, C2, Check, Now) ->
    ID = ecrdt:id(),
    {C1, Mod:add(ID, E, C2), Mod:add(ID, E, Check), Now};
op(Mod, ab, add, E, C1, C2, Check, Now) ->
    ID = ecrdt:id(),
    {Mod:add(ID, E, C1), Mod:add(ID, E, C2), Mod:add(ID, E, Check), Now};
op(Mod, a, remove, E, C1, C2, Check, Now) ->
    case lists:member(E, Mod:value(C1)) of
        true ->
            {Mod:remove(E, C1), C2, Mod:remove(E, Check), Now};
        false ->
            {C1, C2, Check, Now}
    end;
op(Mod, b, remove, E, C1, C2, Check, Now) ->
    case lists:member(E, Mod:value(C2)) of
        true ->
            {C1, Mod:remove(E, C2), Mod:remove(E, Check), Now};
        false ->
            {C1, C2, Check, Now}
    end;
op(Mod, ab, remove, E, C1, C2, Check, Now) ->
    case {lists:member(E, Mod:value(C1)), lists:member(E, Mod:value(C2))} of
        {true, true} ->
            {Mod:remove(E, C1), Mod:remove(E, C2), Mod:remove(E, Check), Now};
        _ ->
            {C1, C2, Check, Now}
    end.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    Obj = new(3),
    lists:foldl(fun({T, O, E}, {A, B, C, M}) ->
                        op(T, O, E, A, B, C, M);
                   (merge, {A, B, C, _}) ->
                        Merged = merge(A, merge(B, C)),
                        {Merged, Merged, Merged, now()};
                   ({gc, a}, {A, B, C, Now}) ->
                        GC = gcable(A, C, Now),
                        {gc_all(A, GC), B, gc_all(C, GC), Now};
                   ({gc, b}, {A, B, C, Now}) ->
                        GC = gcable(B, C, Now),
                        {A, gc_all(B, GC), gc_all(C, GC), Now};
                   ({gc, ab}, {A, B, C, Now}) ->
                        GC = gcable(A, B, C, Now),
                        {gc_all(A, GC), gc_all(B, GC), gc_all(C, GC), Now}
                end, {Obj, Obj, Obj, now()}, Ops).

%% A list of opperations and targets.
targets() ->
    list(weighted_union(
           [{100, {oneof([a, b, ab]), oneof([add, remove]), pos_integer()}},
            {10, {gc, oneof([a, b, ab])}},
            {30, merge}])).

prop_vorsetg() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C, _} = apply_ops(Ts),
                Res = value(merge(merge(B, A), C)) =:= value(merge(merge(A, B), C)),
                ?WHENFAIL(
                   ?debugFmt("~nA = ~p.~nB = ~p.~nC = ~p.~n", [A, B, C]),
                   Res =:= true
                  )
            end).

propper_test() ->
    ?assertEqual([],
                 proper:module(?MODULE,
                               [{to_file, user},
                                long_result,
                                {numtests, 10000}])).

oneof_(L) ->
    lists:nth(random:uniform(length(L)), L).

op(Module, {A, B, C, Now}, Target, Action, Value) ->
    op(Module, Target, Action, Value, A, B, C, Now).

op_fun(_, {A, B}) ->
    case random:uniform(1000) of
        X when X > 200 -> % ADd Remove
            {op(?MODULE, A, oneof_([a, b, ab]), oneof_([add, remove]), X),
             op(vorset, B, oneof_([a, b, ab]), oneof_([add, remove]), X)};
        _X when _X > 50 -> % Merge
            {A1, A2, A3, _} = A,
            Merged = merge(A1, merge(A2, A3)),
            {{Merged, Merged, Merged, now()}, B};
        _X -> % GC
            {A1, A2, A3, Now} = A,
            case oneof_([a, b, ab]) of
                a ->
                    GC = gcable(A1, A3, Now),
                    {{gc_all(A1, GC), A2, gc_all(A3, GC), Now}, B};
                b ->
                    GC = gcable(A2, A3, Now),
                    {{A1, gc_all(A2, GC), gc_all(A3, GC), Now}, B};
                ab ->
                    GC = gcable(A1, A2, A3, Now),
                    {{gc_all(A1, GC), gc_all(A2, GC), gc_all(A3, GC), Now}, B}
            end
    end.

size_test() ->
    random:seed(now()),
    A = new(10),
    B = vorset:new(),
    {{A1, A2, A3, _},
     {B1, B2, B3, _}} =
        lists:foldl(fun op_fun/2,
                    {{A, A, A, now()}, {B, B, B, now()}},
                    lists:seq(1, 1000)),
    AM = merge(A1, merge(A2, A3)),
    BM = vorset:merge(B1, vorset:merge(B2, B3)),
    ?assertEqual(value(AM), vorset:value(BM)).

-endif.

