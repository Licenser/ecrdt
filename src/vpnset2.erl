%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% An implementation of PN Sets based on a undefined set of uniqu
%%% masters.
%%% @end
%%% Created :  8 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(vpnset2).

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([is_a/1, type/0, new/0, add/3, remove/3, merge/2, value/1]).

-type pnset_element() :: {term(), vpncounter2:vpncounter2()}.

-record(vpnset2, {values = [] :: [pnset_element()]}).

-opaque vpnset2() :: #vpnset2{}.

-export_type([vpnset2/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tests is the passed data is implementing this type.
%% @end
%%--------------------------------------------------------------------
-spec is_a(any()) -> true | false.

is_a(#vpnset2{}) ->
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
%% Creates a new empty PN Set.
%% @end
%%--------------------------------------------------------------------
-spec new() -> vpnset2().
new() ->
    #vpnset2{}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the PN set with a given master ID.
%% @end
%%--------------------------------------------------------------------
-spec add(Master::pos_integer(),
          Element::term(),
          Set::vpnset2()) ->
                 Set1::vpnset2().
add(Master, Element,
    #vpnset2{values = Values})  ->
    case lists:keytake(Element, 1, Values) of
        false ->
            #vpnset2{
               values =
                   [{Element,
                     vpncounter2:inc(Master, 1, vpncounter2:new())}
                    | Values]};
        {value, {_, Cnt}, Values1} ->
            #vpnset2{
               values =
                   [{Element,
                     vpncounter2:inc(Master, 1, Cnt)}
                    | Values1]}
    end.
%%--------------------------------------------------------------------
%% @doc
%% Removes an element from the set, this has no effect if this master
%% does not see the element.
%% @end
%%--------------------------------------------------------------------
-spec remove(Master::pos_integer(),
             Element::term(),
             Set::vpnset2()) ->
                    Set1::vpnset2().
remove(Master, Element,
       Set = #vpnset2{values = Values}) ->
    case lists:keytake(Element, 1, Values) of
        false ->
            Set;
        {value, {_, Cnt}, Values1} ->
            case vpncounter2:value(Cnt) of
                X when X > 0 ->
                    #vpnset2{
                       values = [{Element, vpncounter2:dec(Master, 1, Cnt)} | Values1]};
                _ ->
                    Set
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Merges two PN Sets by taking combining the counters of each
%% element. Be aware that for incomplete marges this can end with
%% negative counters!
%% @end
%%--------------------------------------------------------------------
-spec merge(Vpnset20::vpnset2(), Vpnset21::vpnset2()) -> Vpnset2M::vpnset2().
merge(#vpnset2{values = Values1},
      #vpnset2{values = Values2}) ->
    #vpnset2{values = merge_values(lists:keysort(1, Values1),
                                   lists:keysort(1, Values2), [])}.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an PN Set by discarding each
%% element with a count of zero or less.
%% @end
%%--------------------------------------------------------------------
-spec value(Vpnset2::vpnset2()) -> [Element::term()].
value(#vpnset2{values = Values}) ->
    ordsets:from_list(
      lists:foldl(fun ({V, Cnt}, Acc) ->
                          case vpncounter2:value(Cnt) of
                              X when X > 0 ->
                                  [V | Acc];
                              _ ->
                                  Acc
                          end
                  end, [], Values)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec merge_values(Set1::[pnset_element()],
                   Set2::[pnset_element()],
                   Acc::[pnset_element()]) ->
                          [pnset_element()].
merge_values([],
             [], R) ->
    R;

merge_values([],
             R2, R) ->
    R2 ++ R;

merge_values(R1,
             [], R) ->
    R1 ++ R;

merge_values([{V, Cnt1} | R1],
             [{V, Cnt2} | R2], R) ->
    merge_values(R1, R2, [{V, vpncounter2:merge(Cnt1, Cnt2)} | R]);

merge_values([{_V1, _} = E1 | R1],
             [{_V2, _} | _] = R2, R) when _V1 < _V2 ->
            merge_values(R1, R2, [E1 | R]);

merge_values(R1,
             [E2| R2], R) ->
            merge_values(R1, R2, [E2 | R]).


%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, add, E, C1, C2, Check) ->
    ID = 1,
    {add(ID, E, C1), C2, add(ID, E, Check)};
op(b, add, E, C1, C2, Check) ->
    ID = 2,
    {C1, add(ID, E, C2), add(ID, E, Check)};
op(ab, add, E, C1, C2, Check) ->
    ID = 1,
    {add(ID, E, C1), add(ID, E, C2), add(ID, E, Check)};
op(a, remove, E, C1, C2, Check) ->
    case lists:member(E, value(C1)) of
        true ->
            ID = 1,
            {remove(ID, E, C1), C2, remove(ID, E, Check)};
        false ->
            {C1, C2, Check}
    end;
op(b, remove, E, C1, C2, Check) ->
    case lists:member(E, value(C2)) of
        true ->
            ID = 2,
            {C1, remove(ID, E, C2), remove(ID, E, Check)};
        false ->
            {C1, C2, Check}
    end;
op(ab, remove, E, C1, C2, Check) ->
    case {lists:member(E, value(C1)), lists:member(E, value(C2))} of
        {true, true} ->
            ID = 1,
            {remove(ID, E, C1), remove(ID, E, C2), remove(ID, E, Check)};
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

prop_vpnset2() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
