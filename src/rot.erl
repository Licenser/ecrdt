%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% This is not a general purpose data structure, it imeplments a set
%%% that is supposed to take mostly ordered data, in the form of
%%% {ID, Element}, where the ID ideally is some kind of timestamp.
%%%
%%% The datatype is supposed to work as a tool for asyncronous
%%% garbage collection, with older entries slowly timing out with
%%% an agreement about what to delete.
%%% @end
%%% Created :  9 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(rot).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(DEFAULT_SIZE, 100).

-record(rot,
        {size = ?DEFAULT_SIZE,
         elements = [] :: [rot_bucket()]}).

-record(rot_bucket,
        {newest = 0 :: rotid(),
         hash :: hash(),
         elements = []}).

-type rotid() :: {erlang:timestamp(), term()}.
-type hash(IDType) :: {ID::IDType, Hash::binary()}.
-type hash() :: hash(term()).
-opaque rot() :: #rot{}.
-opaque rot_bucket() :: #rot_bucket{}.

-export_type([rot/0, rotid/0, hash/0, hash/1]).

-export([new/0, new/1,
         add/2,
         value/1,
         full/1,
         merge/2,
         remove/2]).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new ROT with a initial ID and default bucket size of 100.
%% @end
%%--------------------------------------------------------------------
-spec new() -> ROT::rot().
new() ->
    #rot{}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new ROT with a initial ID and a given bucket Size.
%% @end
%%--------------------------------------------------------------------
-spec new(Size::pos_integer()) -> ROT::rot().
new(Size) ->
    #rot{size = Size}.

%%--------------------------------------------------------------------
%% @doc
%% Adds a element to the ROT.
%% @end
%%--------------------------------------------------------------------
-spec add({ID::rotid(), Data::term()}, ROT::rot()) -> ROT::rot().
add({Id, Term}, ROT = #rot{elements = Es, size = Size}) ->
    Es1 = case Es of
              [] ->
                  [#rot_bucket{
                      elements = [{Id, Term}],
                      newest = Id
                     }];
              _ ->
                  add(Id, Term, Size, Es, [])
          end,
    ROT#rot{elements = Es1}.

add(Id, Term, Size, [], Acc) ->
    radd(Id, Term, Size, Acc, []);
add(Id, Term, Size, [R], Acc) ->
    {Id1, Term1, R1} = add_element({Id, Term}, Size, R),
    radd(Id1, Term1, Size, Acc, [R1]);
add(Id, Term, Size, [R = #rot_bucket{newest = N} | Rs], Acc) when N =< Id ->
    {Id1, Term1, R1} = add_element({Id, Term}, Size, R),
    radd(Id1, Term1, Size, Acc, [R1 | Rs]);
add(Id, Term, Size, [R | Rs], Acc) ->
    add(Id, Term, Size, Rs, [R | Acc]).

%%--------------------------------------------------------------------
%% @doc
%% Gets the values stored in the rot.
%% @end
%%--------------------------------------------------------------------
-spec value(ROT::rot()) -> [{ID::term(), Value::term()}].
value(#rot{elements = Es}) ->
    lists:sort(lists:flatten([E || #rot_bucket{elements = E} <- Es])).

%%--------------------------------------------------------------------
%% @doc
%% Lists all full buckets with ID and content Hash for compairison.
%% @end
%%--------------------------------------------------------------------
-spec full(ROT::rot()) -> [hash()].
full(#rot{elements = Es}) ->
    [{N, H} || #rot_bucket{
                  newest = N,
                  hash = H} <- Es,
               H =/= undefined].

%%--------------------------------------------------------------------
%% @doc
%% Deletes a bucket with a given Id and Hash.
%% @end
%%--------------------------------------------------------------------

-spec remove(HashID::hash(), ROT::rot()) ->
                    {[Value::term()], ROT::rot()}.
remove([], ROT) ->
    {[], ROT};
remove([ID0 | IDs], ROT) when is_list(IDs) ->
    {Vs, ROT1} = lists:foldl(fun (HashID, {Vs, AccROT}) ->
                                     case remove(HashID, AccROT) of
                                         {[], AccROT1} ->
                                             {Vs, AccROT1};
                                         {Vs1, AccROT1} ->
                                             {[Vs1 | Vs], AccROT1}
                                     end
                             end, remove(ID0, ROT), IDs),
    {lists:flatten(Vs), ROT1};

remove({Newest, Hash},
       ROT = #rot{elements = Es}) ->
    {Vs, Es1} =
        lists:foldl(fun(#rot_bucket{
                           hash = H,
                           newest = N,
                           elements = Elements
                      }, {VAcc, EsAcc}) when H =:= Hash,
                                             N =:= Newest ->
                            {ordsets:union(Elements, VAcc),
                             EsAcc};
                       (E, {VAcc, EsAcc}) ->
                            {VAcc, [E | EsAcc]}
                    end, {[], []}, Es),
    Es2 = lists:reverse(Es1),
    {Vs, ROT#rot{elements = Es2}}.

merge(ROTA, ROTB) ->
    ROTB1 = clean(full(ROTA), ROTB),
    lists:foldl(fun add/2, ROTA, lists:sort(value(ROTB1))).

%%%===================================================================
%%% Internal functions
%%%===================================================================

clean(IDs, ROT = #rot{elements = Es}) ->
    Es1 = lists:filter(fun(#rot_bucket{hash = H, newest = N}) ->
                               not lists:member({N, H}, IDs)
                       end, Es),
    ROT#rot{elements = Es1}.

radd(undefined, undefined, _Size, [], Acc) ->
    Acc;
radd(Id, Term, _Size, [], Acc) ->
    [#rot_bucket{
        elements = [{Id, Term}],
        newest = Id
       } | Acc];
radd(undefined, undefined, Size, [R | Rs], Acc) ->
    radd(undefined, undefined, Size,
         Rs, [R | Acc]);
radd(Id, Term, Size, [R | Rs], Acc) ->
    {Id1, Term1, R1} = add_element({Id, Term}, Size, R),
    radd(Id1, Term1, Size, Rs, [R1 | Acc]).

add_element(E, Size, R = #rot_bucket{elements = Es}) ->
    Es1 = ordsets:add_element(E, Es),
    case length(Es1) of
        _L when _L > Size ->
            Es2 = lists:reverse(Es1),
            [{Id1, Term1} | [{Newest, _} | _] = Es3] = Es2,
            Es4 = lists:reverse(Es3),
            {Id1,
             Term1,
             R#rot_bucket{
               elements = Es4,
               newest = Newest,
               hash = crypto:sha(term_to_binary(Es4))
              }};
        _L when _L =:= Size ->
            Es2 = lists:reverse(Es1),
            [{Newest, _} | _] = Es3 = Es2,
            Es4 = lists:reverse(Es3),
            {undefined,
             undefined,
             R#rot_bucket{
               elements = Es4,
               newest = Newest,
               hash = crypto:sha(term_to_binary(Es4))
              }};
        _ ->
            Es2 = lists:reverse(Es1),
            [{Newest, _} | _] = Es3 = Es2,
            Es4 = lists:reverse(Es3),
            {undefined,
             undefined,
             R#rot_bucket{
               elements = Es4,
               newest = Newest,
               hash = undefined
              }}
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

%% Applies the list of opperaitons to three empty sets.
apply_ops(Values) ->
    lists:foldl(fun({add, E}, ROT) ->
                        Element = {now(), E},
                        add(Element, ROT);
                   (remove, ROT) ->
                        case full(ROT) of
                            [] ->
                                ROT;
                            Full ->
                                R = lists:nth(random:uniform(length(Full)), Full),
                                {_, ROT1} = remove(R, ROT),
                                ROT1
                        end
                end, new(10), Values).

prop_rot_ordset() ->
    ?FORALL(Ts,  list(pos_integer()),
            begin
                {ROT, Orset} =
                    lists:foldl(fun(E, {ROT, Ordset}) ->
                                        Element = {now(), E},
                                        {add(Element, ROT), ordsets:add_element(Element, Ordset)}
                                end, {new(3), ordsets:new()}, Ts),
                Res = ordsets:from_list(value(ROT)) =:= Orset,
                ?WHENFAIL(
                   ?debugFmt("~n~p =/= ~p.~n", [value(ROT), Orset]),
                   Res =:= true
                  )
            end).

prop_rot() ->
    ?FORALL(Ts,
            resize(100, list(
                          weighted_union(
                            [{20, {add, pos_integer()}},
                             {1, remove}]))),
            begin
                apply_ops(Ts),
                true
            end).

propper_test() ->
    ?assertEqual([],
                 proper:module(?MODULE, [{to_file, user},
                                         {numtests, 5000},
                                         long_result])).


-endif.

-ifdef(BLA).
A = rot:add({now(), 4}, rot:add({now(), 3}, rot:add({now(), 2}, rot:add({now(), 1}, rot:new(now(), 2))))).
[F1, F2] = rot:full(A).
rot:remove(F1, A).
{_, A1} = rot:remove(F2, A).
rot:add({now(), 5}, A1).
-endif.
