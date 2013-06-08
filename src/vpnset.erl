-module(vpnset).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, add/3, remove/3, merge/2, value/1]).

-type pnset_element() :: {term(), vpncounter:vpncounter()}.

-record(vpnset, {values = [] :: pnset_element,
                 size :: pos_integer()}).

-opaque vpnset() :: #vpnset{}.

-export_type([vpnset/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty PN Set.
%% @end
%%--------------------------------------------------------------------
-spec new(Size::pos_integer()) -> vpnset().
new(Size) ->
    #vpnset{size = Size}.

%%--------------------------------------------------------------------
%% @doc
%% Adds an element to the PN set with a given master ID.
%% @end
%%--------------------------------------------------------------------
-spec add(Master::pos_integer(),
          Element::term(),
          Set::vpnset()) ->
                 Set1::vpnset().
add(Master, Element,
    #vpnset{values = Values,
            size = Size})  when is_integer(Master),
                                Master > 0,
                                Master =< Size ->
    case lists:keytake(Element, 1, Values) of
        false ->
            #vpnset{
               size = Size,
               values =
                   [{Element,
                     vpncounter:inc(Master, 1, vpncounter:new(Size))}
                    | Values]};
        {value, {_, Cnt}, Values1} ->
            #vpnset{
               size = Size,
               values =
                   [{Element,
                     vpncounter:inc(Master, 1, Cnt)}
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
             Set::vpnset()) ->
                    Set1::vpnset().
remove(Master, Element,
       Set = #vpnset{values = Values,
                     size = Size}) when is_integer(Master),
                                  Master > 0,
                                  Master =< Size ->
    case lists:keytake(Element, 1, Values) of
        false ->
            Set;
        {value, {_, Cnt}, Values1} ->
            case vpncounter:value(Cnt) of
                X when X > 0 ->
                    #vpnset{
                       size = Size,
                       values = [{Element, vpncounter:dec(Master, 1, Cnt)} | Values1]};
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
-spec merge(VPNset0::vpnset(), VPNset1::vpnset()) -> VPNsetM::vpnset().
merge(#vpnset{size = Size, values = Values1},
      #vpnset{size = Size, values = Values2}) ->
    #vpnset{size = Size,
            values = merge_values(lists:keysort(1, Values1),
                                  lists:keysort(1, Values2), [])}.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the list of values from an PN Set by discarding each
%% element with a count of zero or less.
%% @end
%%--------------------------------------------------------------------
-spec value(VPNset::vpnset()) -> [Element::term()].
value(#vpnset{values = Values}) ->
    ordsets:from_list(
      lists:foldl(fun ({V, Cnt}, Acc) ->
                          case vpncounter:value(Cnt) of
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
    merge_values(R1, R2, [{V, vpncounter:merge(Cnt1, Cnt2)} | R]);

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
    ID = 1,
    {remove(ID, E, C1), C2, remove(ID, E, Check)};
op(b, remove, E, C1, C2, Check) ->
    ID = 2,
    {C1, remove(ID, E, C2), remove(ID, E, Check)};
op(ab, remove, E, C1, C2, Check) ->
    ID = 1,
    {remove(ID, E, C1), remove(ID, E, C2), remove(ID, E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    lists:foldl(fun({T, O, E}, {A, B, C}) ->
                        op(T, O, E, A, B, C)
                end, {new(2), new(2), new(2)}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]), oneof([add, remove]), pos_integer()}).

prop_vpnset() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:= value(merge(merge(A, B), C))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).

-endif.
