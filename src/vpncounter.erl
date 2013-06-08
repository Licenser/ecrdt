%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% This implements the NPCounter (negative positive counter) CvRDT
%%% based on two GCounters. This implementation is fixed size.
%%% @end
%%% Created :  1 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(vpncounter).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, value/1, inc/3, dec/3, merge/2]).

-record(vpncounter, {inc_counter :: vgcounter:vgcounter(),
                     dec_counter :: vgcounter:vgcounter(),
                     size :: pos_integer()}).

-opaque vpncounter() :: #vpncounter{}.

-export_type([vpncounter/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty vnpcounter with a given size. Be aware that the
%% size can't be changed later on.
%% @end
%%--------------------------------------------------------------------
-spec new(Size::pos_integer()) -> VPNCounter::vpncounter().
new(Size) when is_integer(Size),
               Size > 0 ->
    #vpncounter{
       inc_counter = vgcounter:new(Size),
       dec_counter = vgcounter:new(Size)
      }.

%%--------------------------------------------------------------------
%% @doc
%% Increments the counter for a given master.
%% @end
%%--------------------------------------------------------------------
-spec inc(Master::pos_integer(),
          Increment::pos_integer(),
          VPNCounter::vpncounter()) ->
                 VPNCounter1::vpncounter().
inc(Master, Increment,
    Counter = #vpncounter{inc_counter = Inc,
                          size = _Size}) when is_integer(Master),
                                              Master > 0,
                                              Master =< _Size ->
    Counter#vpncounter{
      inc_counter = vgcounter:inc(Master, Increment, Inc)
     }.

%%--------------------------------------------------------------------
%% @doc
%% Increments the counter for a given master.
%% @end
%%--------------------------------------------------------------------
-spec dec(Master::pos_integer(),
          Decrement::pos_integer(),
          VNPCounter::vpncounter()) ->
                 VPNCounter1::vpncounter().
dec(Master, Decrement,
    Counter = #vpncounter{dec_counter = Dec,
                          size = _Size}) when is_integer(Master),
                                              Master > 0,
                                              Master =< _Size ->
    Counter#vpncounter{
      dec_counter = vgcounter:inc(Master, Decrement, Dec)
     }.

%%--------------------------------------------------------------------
%% @doc
%% Calculates the final value of the counter.
%% @end
%%--------------------------------------------------------------------
-spec value(VPNCounter :: vpncounter()) -> integer().
value(#vpncounter{dec_counter = Dec,
                  inc_counter = Inc}) ->
    vgcounter:value(Inc) - vgcounter:value(Dec).

%%--------------------------------------------------------------------
%% @doc
%% Merges to PNCounters, by keeping the maximum known value for each
%% master for increments and decrements.
%% @end
%%--------------------------------------------------------------------
-spec merge(VPNCounter1::vpncounter(), VGCounter2::vpncounter()) ->
                   VPNCounter::vpncounter().
merge(#vpncounter{dec_counter = Dec0,
                  inc_counter = Inc0,
                  size = Size},
      #vpncounter{dec_counter = Dec1,
                  inc_counter = Inc1,
                  size = Size}) ->
    #vpncounter{dec_counter = vgcounter:merge(Dec0, Dec1),
                inc_counter = vgcounter:merge(Inc0, Inc1),
                size = Size}.

-ifdef(TEST).

op(a, inc, E, C1, C2, Check) ->
    {inc(1, E, C1), C2, inc(1, E, Check)};

op(b, inc, E, C1, C2, Check) ->
    {C1, inc(2, E, C2), inc(2, E, Check)};

op(ab1, inc, E, C1, C2, Check) ->
    {inc(1, E, C1), inc(1, E, C2), inc(1, E, Check)};

op(ab2, inc, E, C1, C2, Check) ->
    {inc(2, E, C1), inc(2, E, C2), inc(2, E, Check)};

op(a, dec, E, C1, C2, Check) ->
    {dec(1, E, C1), C2, dec(1, E, Check)};

op(b, dec, E, C1, C2, Check) ->
    {C1, dec(2, E, C2), dec(2, E, Check)};

op(ab1, dec, E, C1, C2, Check) ->
    {dec(1, E, C1), dec(1, E, C2), dec(1, E, Check)};

op(ab2, dec, E, C1, C2, Check) ->
    {dec(2, E, C1), dec(2, E, C2), dec(2, E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, E}, {A, B, C}) ->
                        op(T, O, E, A, B, C)
                end, {new(2), new(2), new(2)}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab1, ab2]), oneof([inc, dec]),  pos_integer()}).

prop_vpncounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                C =:= merge(A, B)
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
