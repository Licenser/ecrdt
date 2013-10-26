%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% An implementation of the GCounter (grow only counter) CvRDT using
%%% an known set of masters identified by a direct mapping to integer
%%% id's (1...N).
%%% @end
%%% Created :  1 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(vgcounter).

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([is_a/1, type/0, new/0, new/1, value/1, inc/3, merge/2]).

-record(vgcounter, {vector, size}).

-opaque vgcounter() :: #vgcounter{}.

-export_type([vgcounter/0]).

-define(DEF_SIZE, 16).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tests is the passed data is implementing this type.
%% @end
%%--------------------------------------------------------------------
-spec is_a(any()) -> true | false.

is_a(#vgcounter{}) ->
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
    gcounter.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty vgcounter with a given size. Be aware that the
%% size can't be changed later on.
%% @end
%%--------------------------------------------------------------------
-spec new(Size::pos_integer()) -> VGCounter::vgcounter().
new(Size) ->
    L = [ 0 || _ <- lists:seq(1, Size)],
    #vgcounter{size = Size, vector = list_to_tuple(L)}.

new() ->
    new(?DEF_SIZE).

%%--------------------------------------------------------------------
%% @doc
%% Increments the counter for a given master.
%% @end
%%--------------------------------------------------------------------
-spec inc(Master::pos_integer(),
          Increment::pos_integer(),
          VGCounter::vgcounter()) ->
                 VGCounter1::vgcounter().
inc(Master, Increment,
    Counter = #vgcounter{vector = Vector0,
                         size = _Size}) when Increment > 0,
                                             Master =< _Size ->
    Value0 = element(Master, Vector0),
    Value1 = Value0 + Increment,
    Vector1 = setelement(Master, Vector0, Value1),
    Counter#vgcounter{vector = Vector1}.

%%--------------------------------------------------------------------
%% @doc
%% Merges to GCounters, by keeping the maximum known value for each
%% master.
%% @end
%%--------------------------------------------------------------------
-spec merge(VGCounter1::vgcounter(), VGCounter2::vgcounter()) ->
                   VGCounter::vgcounter().
merge(Counter0 = #vgcounter{vector = Vector0, size = _Size},
      #vgcounter{vector = Vector1, size = _Size}) ->
    VectorM = list_to_tuple(
                merge_vecotrs(tuple_to_list(Vector0),
                              tuple_to_list(Vector1),
                              [])),
    Counter0#vgcounter{vector = VectorM}.

%%--------------------------------------------------------------------
%% @doc
%% Compiles the value of the counter by summing up all master values.
%% @end
%%--------------------------------------------------------------------
-spec value(VGCounter::vgcounter()) -> Value::pos_integer().
value(#vgcounter{vector = Vector0}) ->
    lists:sum(tuple_to_list(Vector0)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

merge_vecotrs([V0 | R0], [_V1 | R1], Acc) when V0 >= _V1 ->
    merge_vecotrs(R0, R1, [V0 | Acc]);
merge_vecotrs([_ | R0], [V1 | R1], Acc) ->
    merge_vecotrs(R0, R1, [V1 | Acc]);
merge_vecotrs([], [], Acc) ->
    lists:reverse(Acc).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, E, C1, C2, Check) ->
    {inc(1, E, C1), C2, inc(1, E, Check)};

op(b, E, C1, C2, Check) ->
    {C1, inc(2, E, C2), inc(2, E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, E}, {A, B, C}) ->
                        op(T, E, A, B, C)
                end, {new(2), new(2), new(2)}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b]), pos_integer()}).

prop_vgcounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
