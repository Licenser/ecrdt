-module(vpncounter).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, value/1, inc/3, dec/3, merge/2]).

-record(vpncounter, {inc_counter, dec_counter}).


new(Size) ->
    #vpncounter{
       inc_counter = vgcounter:new(Size),
       dec_counter = vgcounter:new(Size)
      }.

inc(Master, Increment,
    Counter = #vpncounter{inc_counter = Inc}) ->
    Counter#vpncounter{
      inc_counter = vgcounter:inc(Master, Increment, Inc)
     }.

dec(Master, Increment,
    Counter = #vpncounter{dec_counter = Dec}) ->
    Counter#vpncounter{
      dec_counter = vgcounter:inc(Master, Increment, Dec)
     }.

value(#vpncounter{dec_counter = Dec,
                 inc_counter = Inc}) ->
    vgcounter:value(Inc) - vgcounter:value(Dec).

merge(#vpncounter{dec_counter = Dec0,
                 inc_counter = Inc0},
      #vpncounter{dec_counter = Dec1,
                 inc_counter = Inc1}) ->
    #vpncounter{dec_counter = vgcounter:merge(Dec0, Dec1),
               inc_counter = vgcounter:merge(Inc0, Inc1)}.

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
