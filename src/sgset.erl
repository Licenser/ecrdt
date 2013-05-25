-module(sgset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, add/2, merge/2, value/1]).

-record(sgset, {payload = []}).

-opaque sgset() :: #sgset{}.

-export_type([sgset/0]).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty Sgset (only growing set)
%% @end
%%--------------------------------------------------------------------

-spec new() -> sgset().

new() ->
    #sgset{}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the Sgset.
%% @end
%%--------------------------------------------------------------------
-spec add(Element::term(), Sgset::sgset()) -> Sgset1::sgset().

add(Element, #sgset{payload = Payload}) ->
    #sgset{payload = ordsets:add_element(Element, Payload)}.

%%--------------------------------------------------------------------
%% @doc
%% Merges tow Sgsets.
%% @end
%%--------------------------------------------------------------------
-spec merge(Sgset1::sgset(), Sgset2::sgset()) -> Sgset::sgset().

merge(#sgset{payload = Payload1}, #sgset{payload = Payload2}) ->
    #sgset{payload = ordsets:union(Payload1, Payload2)}.


%%--------------------------------------------------------------------
%% @doc
%% Gets the raw value of a sgset.
%% @end
%%--------------------------------------------------------------------
-spec value(Sgset::sgset()) -> [Element::term()].

value(#sgset{payload = Payload}) ->
    Payload.


%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

%% Apply a option either to the first set the second one or both.
%% Also keeps track of all changes in a check set (3rd).
op(a, E, Sgset1, Sgset2, Check) ->
    {add(E, Sgset1), Sgset2, add(E, Check)};

op(b, E, Sgset1, Sgset2, Check) ->
    {Sgset1, add(E, Sgset2), add(E, Check)};

op(ab, E, Sgset1, Sgset2, Check) ->
    {add(E, Sgset1), add(E, Sgset2), add(E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, E}, {A, B, C}) ->
                        op(T, E, A, B, C)
                end, {new(), new(), new()}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), integer()}).

prop_sgset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                C =:= merge(A, B)
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
