-module(vormap).

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         new/0,
         set/3,
         remove/2,
         value/1,
         merge/2,
         is_a/1,
         type/0
        ]).

-record(vormap, {
          set_backend = vorsetg,
          reg_backend = vlwwregister,
          keys :: any(),
          map = []
         }).

-opaque vormap() :: #vormap{}.

-export_type([vormap/0]).

new() ->
    init(#vormap{}).

is_a(#vormap{}) ->
    true;

is_a(_) ->
    false.

type() ->
    map.

set(Ks, V, M = #vormap{
                  reg_backend = Reg
                 }) when is_list(Ks) ->
    Fun = fun(undefined) ->
                  Reg:new(V);
             (E) ->
                  case ecrdt:type(E) of
                      register ->
                          Reg:assign(V, E);
                      _ ->
                          V
                  end
          end,
    update(Ks, Fun, M);

set(K, V, M) ->
    set([K], V, M).

remove([K], M) ->
    remove(K, M);

remove(Ks, M) when is_list(Ks) ->
    Fun = fun(_) ->
                  remove
          end,
    update(Ks, Fun, M);

remove(K,
       M = #vormap{
              keys = Keys,
              map = Map,
              set_backend = B
             }) ->
    Keys1 = B:remove(K, Keys),
    Map1 = orddict:erase(K, Map),
    M#vormap{
      keys = Keys1,
      map = Map1
     }.


update([K], Fun,
       M = #vormap{
              keys = Keys,
              map = Map,
              set_backend = B
             }) ->
    {V, Keys1} =
        case orddict:find(K, Map) of
            error ->
                {undefined, Keys};
            {ok, V0} ->
                {V0, B:remove(K, Keys)}
        end,
    case Fun(V) of
        remove ->
            M#vormap{
              keys = Keys1,
              map = orddict:erase(K, Map)
             };
        V1 ->
            Keys2 = B:add(K, Keys1),
            Map1 = orddict:store(K, V1, Map),
            M#vormap{
              keys = Keys2,
              map = Map1
             }
    end;

update([K | Ks], Fun,
       M = #vormap{
              keys = Keys,
              map = Map,
              set_backend = B
             }) ->
    {V, Keys1} =
        case orddict:find(K, Map) of
            error ->
                {clone(M), Keys};
            {ok, V0 = #vormap{}} ->
                {V0, B:remove(K, Keys)}
        end,
    Keys2 = B:add(K, Keys1),
    V1 = update(Ks, Fun, V),
    Map1 = orddict:store(K, V1, Map),
    M#vormap{
      keys = Keys2,
      map = Map1
     }.

value(#vormap{map = M}) ->
    [{K, ecrdt:value(V)} || {K, V} <- M].

merge(M0 = #vormap{
              set_backend = Set,
              reg_backend = _R,
              keys = KeysA,
              map = MapA
             },
      #vormap{
         set_backend = Set,
         reg_backend = _R,
         keys = KeysB,
         map = MapB
        }) ->
    KeysM = Set:merge(KeysA, KeysB),
    KeysMv = ordsets:from_list(Set:value(KeysM)),
    DeleteA = keys_to_delete(ordsets:from_list(Set:value(KeysA)), KeysMv),
    DeleteB = keys_to_delete(ordsets:from_list(Set:value(KeysB)), KeysMv),
    MapA0 = lists:foldl(fun orddict:erase/2, MapA, DeleteA),
    MapB0 = lists:foldl(fun orddict:erase/2,  MapB, DeleteB),
    Map = merge_maps(MapA0, MapB0),
    io:format("~p~n", [Set:value(KeysM)]),
    M0#vormap{
      keys = KeysM,
      map = Map
     }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_maps([], []) ->
    [];

merge_maps([A | Ra], []) ->
    [A | merge_maps(Ra, [])];

merge_maps([{K, Va} | Ra], [{K, Vb} | Rb]) ->
    [{K, ecrdt:merge(Va, Vb)} | merge_maps(Ra, Rb)];

merge_maps([{Ka, _} = A | Ra], [{Kb, _}| _] = Rb) when Ka < Kb ->
    [A | merge_maps(Ra, Rb)];

merge_maps(Ra, [B | Rb]) ->
    [B | merge_maps(Ra, Rb)].

keys_to_delete(Keys, KeysM) ->
    ordsets:subtract(Keys, KeysM).

clone(#vormap{} = V) ->
    init(V).

init(#vormap{set_backend = B}) ->
    #vormap{
       keys = B:new(),
       map = []
      }.

-ifdef(TEST).
new_test() ->
    M = vormap:new(),
    ?assertEqual([], value(M)).

set_test() ->
    M = vormap:new(),
    M1 = vormap:set(a, 1, M),
    M2 = vormap:set(b, 2, M1),
    M3 = vormap:set(a, 3, M2),
    M4 = vormap:remove(b, M3),
    ?assertEqual([{a, 1}], value(M1)),
    ?assertEqual([{a, 1}, {b, 2}], value(M2)),
    ?assertEqual([{a, 3}, {b, 2}], value(M3)),
    ?assertEqual([{a, 3}], value(M4)).

nested_test() ->
    M = vormap:new(),
    M1 = vormap:set([o, a], 1, M),
    M2 = vormap:set([o, b], 2, M1),
    M3 = vormap:set([o, a], 3, M2),
    M4 = vormap:remove([o, b], M3),
    ?assertEqual([{o, [{a, 1}]}], value(M1)),
    ?assertEqual([{o, [{a, 1}, {b, 2}]}], value(M2)),
    ?assertEqual([{o, [{a, 3}, {b, 2}]}], value(M3)),
    ?assertEqual([{o, [{a, 3}]}], value(M4)),
    ok.

merge_test() ->
    M = vormap:new(),
    M1 = vormap:set(a, 1, M),
    M2 = vormap:set(b, 2, M1),
    M3 = vormap:set(a, 3, M2),
    M4 = vormap:remove(b, M3),
    MM1 = merge(M4, M3),
    MM1a = merge(M3, M4),
    MM2 = merge(M1, M4),
    MM2a = merge(M4, M1),
    ?assertEqual(value(MM1), value(M4)),
    ?assertEqual(value(MM1a), value(M4)),
    ?assertEqual(value(MM2), value(M4)),
    ?assertEqual(value(MM2a), value(M4)),
    ok.

nested_merge_test() ->
    M = vormap:new(),
    M1 = vormap:set([o, a], 1, M),
    M2 = vormap:set([o, b], 2, M1),
    M3 = vormap:set([o, a], 3, M2),
    M4 = vormap:remove([o, b], M3),
    MM1 = merge(M4, M3),
    MM1a = merge(M3, M4),
    MM2 = merge(M1, M4),
    MM2a = merge(M4, M1),
    ?assertEqual(value(MM1), value(M4)),
    ?assertEqual(value(MM1a), value(M4)),
    ?assertEqual(value(MM2), value(M4)),
    ?assertEqual(value(MM2a), value(M4)),
    ok.

merge_maps_test() ->
    M = [{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}],
    Ma = [{a, 1}, {b, 2}, {c,3}, {e, 5}],
    Mb = [{a, 1}, {c, 3}, {d, 4}, {e, 5}],
    Mm = from_reg(merge_maps(to_reg(Ma), to_reg(Mb))),
    ?assertEqual(M, Mm).


to_reg(M) ->
    [{K, vlwwregister:new(V)} || {K, V} <- M].

from_reg(M) ->
    [{K, vlwwregister:value(V)} || {K, V} <- M].


-endif.
