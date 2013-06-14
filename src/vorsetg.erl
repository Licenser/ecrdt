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
         remove/3,
         merge/2,
         value/1,
         gc/2, gcable/1
        ]).

-record(vorsetg, {adds = [] :: ordsets:ordset(),
                  gced = [] :: ordsets:ordset(),
                  removes :: rot:rot()}).

-opaque vorsetg() :: #vorsetg{}.

-define(NUMTESTS, 500).
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
    #vorsetg{removes = rot:new()}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty OR Set with a given bucket size for the GC rot.
%% @end
%%--------------------------------------------------------------------
-spec new(Size::pos_integer()) -> vorsetg().
new(Size) ->
    #vorsetg{removes = rot:new(Size)}.

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
-spec remove(Id :: term(), Element::term(), ORSet::vorsetg()) -> ORSet1::vorsetg().
remove(Id, Element, ORSet = #vorsetg{removes = Removes}) ->
    CurrentExisting = [Elem || Elem = {_, E1} <- raw_value(ORSet),
                               E1 =:= Element],
    Removes1 = lists:foldl(fun(R, Rs) ->
                                   rot:add({Id, R}, Rs)
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
    ROT1 = rot:merge(RemovesA, RemovesB),
    #vorsetg{adds = ordsets:union(AddsA, AddsB),
             gced = GCed,
             removes = ROT1}.

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
    Values1 = [V || {_, V} <- Values],
    Values2 = ordsets:from_list(Values1),
    #vorsetg{adds = ordsets:subtract(Adds, Values2),
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
    ordsets:subtract(Adds, lists:sort([E || {_, E} <- rot:value(Removes)])).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
%%random_element(L) ->
%%    lists:nth(random:uniform(length(L)), L).

now_us() ->
    {MegaSecs,Secs,MicroSecs} = now(),
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

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
op(Mod, a, remove, _E0, C1, C2, Check, Now) ->
    case ordsets:from_list(Mod:value(C1)) of
        [] ->
            {C1, C2, Check, Now};
        [E | _] ->
            case Mod of
                ?MODULE ->
                    ID = now_us(),
                    {Mod:remove(ID, E, C1), C2, Mod:remove(ID, E, Check), Now};
                _ ->
                    {Mod:remove(E, C1), C2, Mod:remove(E, Check), Now}
            end
    end;
op(Mod, b, remove, _E0, C1, C2, Check, Now) ->
    case ordsets:from_list(Mod:value(C2)) of
        [] ->
            {C1, C2, Check, Now};
        [E | _] ->
            case Mod of
                ?MODULE ->
                    ID = now_us(),
                    {C1, Mod:remove(ID, E, C2), Mod:remove(ID, E, Check), Now};
                _ ->
                    {C1, Mod:remove(E, C2), Mod:remove(E, Check), Now}
            end
    end;
op(Mod, ab, remove, _E0, C1, C2, Check, Now) ->
    Vs1 = ordsets:from_list(Mod:value(C1)),
    Vs2 = ordsets:from_list(Mod:value(C2)),
    case ordsets:intersection(Vs1, Vs2) of
        [] ->
            {C1, C2, Check, Now};
        [E | _] ->
            case Mod of
                ?MODULE ->
                    ID = now_us(),
                    {Mod:remove(ID, E, C1),
                     Mod:remove(ID, E, C2),
                     Mod:remove(ID, E, Check), Now};
                _ ->
                    {Mod:remove(E, C1),
                     Mod:remove(E, C2),
                     Mod:remove(E, Check), Now}
            end
    end.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    Obj = new(3),
    lists:foldl(fun({T, O, E}, {A, B, C, M}) ->
                        op(T, O, E, A, B, C, M);
                   (merge, {A, B, C, _}) ->
                        Merged = merge(A, merge(B, C)),
                        {Merged, Merged, Merged, now_us()};
                   ({gc, a}, {A, B, C, Now}) ->
                        GC = gcable(A, C, Now),
                        {gc_all(A, GC), B, gc_all(C, GC), Now};
                   ({gc, b}, {A, B, C, Now}) ->
                        GC = gcable(B, C, Now),
                        {A, gc_all(B, GC), gc_all(C, GC), Now};
                   ({gc, ab}, {A, B, C, Now}) ->
                        GC = gcable(A, B, C, Now),
                        {gc_all(A, GC), gc_all(B, GC), gc_all(C, GC), Now}
                end, {Obj, Obj, Obj, now_us()}, Ops).

%% A list of opperations and targets.
targets() ->
    list(weighted_union(
           [{7, {oneof([a, b, ab]), oneof([add, remove]), integer(500, 550)}},
            {1, {gc, oneof([a, b, ab])}},
            {3, merge}])).

prop_vorsetg() ->
    ?FORALL(Ts,  resize(1, targets()),
            begin
                {A, B, C, _} = apply_ops(Ts),
                ?WHENFAIL(
                   ?debugFmt("~nA = ~p.~nB = ~p.~nC = ~p.~n", [A, B, C]),
                   value(merge(merge(B, A), C)) =:=
                       value(merge(merge(A, B), C))
                   andalso
                   value(merge(A, B)) =:=
                       value(merge(A, B)))
            end).

op(Module, {A, B, C, Now}, Target, Action, Value) ->
    op(Module, Target, Action, Value, A, B, C, Now).


targets_cmp() ->
    list(weighted_union(
           [{7, {oneof([a, b, ab]), oneof([add, remove]), integer(500, 550)}},
            {1, gc},
            {3, merge}])).

term_size(T) ->
    byte_size(term_to_binary(T)).

size_check(N) ->
    Aggr = spawn(
             fun () ->
                     process_flag(trap_exit, true),
                     aggregator({[], [], [], [], []})
             end),
    ?FORALL(Ts,
            resize(N, targets_cmp()),
            begin
                A0 = new(5),
                B0 = vorset:new(),
                Now0 = now_us(),
                {{A1, A2, _A3, _},
                 {B1, B2, _B3, _}} =
                    lists:foldl(
                      fun({T, O, E}, {A, B}) ->
                              {T0, R} = timer:tc(
                                          fun() ->
                                                  %%E1 = <<0:(8*E)>>,
                                                  E1 = E,
                                                  A1 = op(?MODULE, A, T, O, E1),
                                                  B1 = op(vorset, B, T, O, E1),
                                                  {A1,B1}
                                          end),
                              Aggr ! {op, T0},
                              R;
                         (merge, {{A1, A2, _A3, _}, {B1, B2, _B3, _}}) ->
                              {T0, Merged} = timer:tc(
                                               fun() ->
                                                       merge(A1, A2)
                                               end),
                              {T1, MergedB} = timer:tc(
                                                fun() ->
                                                        vorset:merge(B1, B2)
                                                end),
                              Aggr ! {merge, T0 + T1},
                              {{Merged, Merged, Merged, now_us()},
                               {MergedB, MergedB, MergedB, now_us()}};
                         (gc, {{A1, A2, A3, Now}, B}) ->
                              {T0, R} =
                                  timer:tc(
                                    fun() ->
                                            GC = gcable(A1, A2, Now),
                                            A1g = gc_all(A1, GC),
                                            A2g = gc_all(A2, GC),
                                            Aggr ! {reduction,
                                                    (term_size(A1g) +
                                                         term_size(A2g)) /
                                                        (term_size(A1) +
                                                             term_size(A2))},
                                            {{A1g, A2g, A3, Now}, B}
                                    end),
                              Aggr ! {gc, T0},
                              R
                      end, {{A0, A0, A0, Now0},
                            {B0, B0, B0, Now0}}, Ts),
                AM = merge(A1, A2),
                BM = vorset:merge(B1, B2),
                AR = lists:sort(value(AM)),
                BR = lists:sort(vorset:value(BM)),
                AS = byte_size(term_to_binary(AM)),
                BS = byte_size(term_to_binary(BM)),
                Aggr ! {add, AS/BS},
                ?WHENFAIL(
                   ?debugFmt("~n~p =/= ~p.~n", [AR, BR]),
                   begin
                       AR =:= BR
                   end)
            end).

prop_vorset_cmp() ->
    size_check(1000).


propper_test_() ->
    {timeout, 240,
     ?_assertEqual([],
                   proper:module(?MODULE,
                                 [{to_file, user},
                                  long_result,
                                  {numtests, ?NUMTESTS}]))}.

show_stats(Title, Vs) ->
    case Vs of
        [] ->
            ?debugFmt("[~s] Cnt: ~p, Avg: ~p, Min: ~p, Max: ~p~n",
                      [Title, 0, 0, 0, 0]);
        _ ->
            Cnt = length(Vs),
            Avg = lists:sum(Vs) / Cnt,
            Max = lists:max(Vs),
            Min = lists:min(Vs),
            ?debugFmt("[~s] Cnt: ~p, Avg: ~p, Min: ~p, Max: ~p~n",
                      [Title, Cnt, Avg, Min, Max])
    end.

aggregator({Vs, GC, M, Op, Rs}) ->
    receive
        {add, V} ->
            Vs1 = [V | Vs],
            case length(Vs1) of
                ?NUMTESTS ->
                    show_stats("Size", Vs),
                    show_stats(" GC ", GC),
                    show_stats(" RS ", Rs),
                    show_stats(" MG ", M),
                    show_stats(" OP ", Op);
                _ ->
                    aggregator({Vs1, GC, M, Op, Rs})
            end;
        {gc, V} ->
            aggregator({Vs, [V | GC], M, Op, Rs});
        {merge, V} ->
            aggregator({Vs, GC, [V | M], Op, Rs});
        {op, V} ->
            aggregator({Vs, GC, M, [V | Op], Rs});
        {reduction, V} ->
            aggregator({Vs, GC, M, Op, [V | Rs]});
        done ->
            show_stats("Size", Vs),
            show_stats(" RS ", Rs),
            show_stats(" GC ", GC),
            show_stats(" MG ", M),
            show_stats(" OP ", Op)
    end.


-endif.
