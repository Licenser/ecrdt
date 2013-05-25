-module(mmcounter).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, value/1, inc/3, dec/3, merge/2]).

-record(mmcounter, {inc_counter, dec_counter}).


new(Size) ->
    #mmcounter{
       inc_counter = mmicounter:new(Size),
       dec_counter = mmicounter:new(Size)
      }.

inc(Master, Increment,
    Counter = #mmcounter{inc_counter = Inc}) ->
    Counter#mmcounter{
      inc_counter = mmicounter:inc(Master, Increment, Inc)
     }.

dec(Master, Increment,
    Counter = #mmcounter{dec_counter = Dec}) ->
    Counter#mmcounter{
      dec_counter = mmicounter:inc(Master, Increment, Dec)
     }.

value(#mmcounter{dec_counter = Dec,
                 inc_counter = Inc}) ->
    mmicounter:value(Inc) - mmicounter:value(Dec).

merge(#mmcounter{dec_counter = Dec0,
                 inc_counter = Inc0},
      #mmcounter{dec_counter = Dec1,
                 inc_counter = Inc1}) ->
    #mmcounter{dec_counter = mmicounter:merge(Dec0, Dec1),
               inc_counter = mmicounter:merge(Inc0, Inc1)}.


-ifdef(TEST).

op(a, inc, E, C1, C2, Check) ->
    {inc(1, E, C1), C2, inc(1, E, Check)};

op(b, inc, E, C1, C2, Check) ->
    {C1, inc(2, E, C2), inc(2, E, Check)};

op(a, dec, E, C1, C2, Check) ->
    {dec(1, E, C1), C2, dec(1, E, Check)};

op(b, dec, E, C1, C2, Check) ->
    {C1, dec(2, E, C2), dec(2, E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, E}, {A, B, C}) ->
                        op(T, O, E, A, B, C)
                end, {new(2), new(2), new(2)}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b]), oneof([inc, dec]),  pos_integer()}).

prop_mmicounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                C =:= merge(A, B)
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
