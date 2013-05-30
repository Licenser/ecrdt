-module(vpncounter2).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, value/1, inc/3, dec/3, merge/2]).

-record(vpncounter2, {inc_counter, dec_counter}).


new() ->
    #vpncounter2{
       inc_counter = vgcounter2:new(),
       dec_counter = vgcounter2:new()
      }.

inc(Master, Increment,
    Counter = #vpncounter2{inc_counter = Inc}) ->
    Counter#vpncounter2{
      inc_counter = vgcounter2:inc(Master, Increment, Inc)
     }.

dec(Master, Increment,
    Counter = #vpncounter2{dec_counter = Dec}) ->
    Counter#vpncounter2{
      dec_counter = vgcounter2:inc(Master, Increment, Dec)
     }.

value(#vpncounter2{dec_counter = Dec,
                   inc_counter = Inc}) ->
    vgcounter2:value(Inc) - vgcounter2:value(Dec).

merge(#vpncounter2{dec_counter = Dec0,
                   inc_counter = Inc0},
      #vpncounter2{dec_counter = Dec1,
                   inc_counter = Inc1}) ->
    #vpncounter2{dec_counter = vgcounter2:merge(Dec0, Dec1),
                 inc_counter = vgcounter2:merge(Inc0, Inc1)}.

-ifdef(TEST).

op(a, inc, E, C1, C2, Check) ->
    {inc(a, E, C1), C2, inc(a, E, Check)};

op(b, inc, E, C1, C2, Check) ->
    {C1, inc(b, E, C2), inc(b, E, Check)};

op(ab1, inc, E, C1, C2, Check) ->
    {inc(a, E, C1), inc(a, E, C2), inc(a, E, Check)};

op(ab2, inc, E, C1, C2, Check) ->
    {inc(b, E, C1), inc(b, E, C2), inc(b, E, Check)};

op(a, dec, E, C1, C2, Check) ->
    {dec(a, E, C1), C2, dec(a, E, Check)};

op(b, dec, E, C1, C2, Check) ->
    {C1, dec(b, E, C2), dec(b, E, Check)};

op(ab1, dec, E, C1, C2, Check) ->
    {dec(a, E, C1), dec(a, E, C2), dec(a, E, Check)};

op(ab2, dec, E, C1, C2, Check) ->
    {dec(b, E, C1), dec(b, E, C2), dec(b, E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    Obj = new(),
    lists:foldl(fun({T, O, E}, {A, B, C}) ->
                        op(T, O, E, A, B, C)
                end, {Obj, Obj, Obj}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab1, ab2]), oneof([inc, dec]),  pos_integer()}).

prop_vpncounter2() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
