-module(vormap).

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         new/0, new/1, is_a/1, type/0, merge/2,
         set/3, remove/2,
         inc/3, dec/3,
         set_add/3, set_remove/3,
         value/1, get/2, get_value/2
        ]).

-record(vormap, {
          set_backend = vorset2,
          reg_backend = vlwwregister,
          cnt_backend = vpncounter2,
          keys :: term(),
          map = []
         }).

-type vormpa_opts():: [{set, SetBackend::atom()} |
                       {counter, CounterBackend::atom()} |
                       {register, RegisterBackend::atom()}].

-type update_fn() :: fun((V0::term() | undefined) ->
                                V1 :: term() | delete).
-opaque vormap() :: #vormap{}.

-export_type([vormap/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

-spec new() -> vormap().
new() ->
    init(#vormap{}).

-spec new(Opts::vormpa_opts()) -> vormap().
new(Opts) ->
    new(Opts, #vormap{}).

new([{set, B} | R], M) ->
    new(R, M#vormap{set_backend = B});

new([{register, B} | R], M) ->
    new(R, M#vormap{reg_backend = B});

new([{counter, B} | R], M) ->
    new(R, M#vormap{cnt_backend = B});

new([], M) ->
    init(M).

-spec is_a(term()) -> true | false.
is_a(#vormap{}) ->
    true;

is_a(_) ->
    false.

-spec type() -> register | set | gset | counter | gcounter | map.
type() ->
    map.

-spec set(Ks::[term()]|term(), V::term(), M::vormap()) -> vormap().
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

-spec remove(Ks::[term()]|term(), M::vormap()) -> vormap().

remove([K], M) ->
    remove(K, M);

remove(Ks, M) when is_list(Ks) ->
    Fun = fun(_) ->
                  delete
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

-spec inc(Ks::[term()]|term(), V::pos_integer(), M::vormap()) ->
                 vormap().
inc(Ks, V, M = #vormap{
                  cnt_backend = Cnt
                 }) when is_list(Ks),
                         V >= 0 ->
    Fun = fun(undefined) ->
                  Cnt:inc(V, Cnt:new());
             (E) ->
                  case ecrdt:type(E) of
                      counter ->
                          Cnt:inc(V, E);
                      gcounter ->
                          Cnt:inc(V, E);
                      _ ->
                          erlang:error(badarg)
                  end
          end,
    update(Ks, Fun, M);

inc(K, V, M) when V >= 0->
    inc([K], V, M).

-spec dec(Ks::[term()]|term(), V::pos_integer(), M::vormap()) ->
                 vormap().
dec(Ks, V, M = #vormap{
                  cnt_backend = Cnt
                 }) when is_list(Ks),
                         V >= 0->
    Fun = fun(undefined) ->
                  Cnt:dec(V, Cnt:new());
             (E) ->
                  case ecrdt:type(E) of
                      counter ->
                          Cnt:dec(V, E);
                      _ ->
                          erlang:error(badarg)
                  end
          end,
    update(Ks, Fun, M);

dec(K, V, M) when V >= 0 ->
    dec([K], V, M).

-spec set_add(Ks::[term()]|term(), V::term(), M::vormap()) ->
                     vormap().
set_add(Ks, V, M = #vormap{
                      set_backend = Set
                     }) when is_list(Ks) ->
    Fun = fun(undefined) ->
                  Set:from_list([V]);
             (E) ->
                  case ecrdt:type(E) of
                      gset ->
                          Set:add(V, E);
                      set ->
                          Set:add(V, E);
                      _ ->
                          erlang:error(badarg)
                  end
          end,
    update(Ks, Fun, M);

set_add(K, V, M) ->
    set_add([K], V, M).

-spec set_remove(Ks::[term()]|term(), V::term(), M::vormap()) ->
                        vormap().
set_remove(Ks, V, M = #vormap{
                         set_backend = Set
                        }) when is_list(Ks) ->
    Fun = fun(undefined) ->
                  Set:from_list([]);
             (E) ->
                  case ecrdt:type(E) of
                      set ->
                          Set:remove(V, E);
                      _ ->
                          erlang:error(badarg)
                  end
          end,
    update(Ks, Fun, M);

set_remove(K, V, M) ->
    set_remove([K], V, M).

-spec update(Ks::[term()]|term(),
             F::update_fn(), M::vormap()) ->
                    vormap().
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
        delete ->
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


-spec get_value(Ks::[term()]|term(), vormap()) -> term().

get_value(Ks, M) ->
    case vormap:get(Ks, M) of
        {ok, V} ->
            {ok, ecrdt:value(V)};
        E ->
            E
    end.

-spec get(Ks::[term()]|term(), vormap()) -> term().

get([K], #vormap{map = M}) ->
    orddict:find(K, M);

get([K | Ks], #vormap{map = M}) ->
    case orddict:find(K, M) of
        {ok, M1} ->
            vormap:get(Ks, M1);
        E ->
            E
    end;
get(K, M) ->
    vormap:get([K], M).

-spec value(vormap()) -> orddict:orddict().
value(#vormap{map = M}) ->
    [{K, ecrdt:value(V)} || {K, V} <- M].

-spec merge(A::vormap(), B::vormap()) ->
                   Merged::vormap().
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

get_test() ->
    M = vormap:new(),
    M1 = vormap:set(a, 1, M),
    M2 = vormap:set(b, 2, M1),
    M3 = vormap:set(a, 3, M2),
    M4 = vormap:remove(b, M3),
    ?assertEqual({ok, 1}, get_value(a, M1)),
    ?assertEqual({ok, 1}, get_value(a, M2)),
    ?assertEqual({ok, 2}, get_value(b, M2)),
    ?assertEqual({ok, 3}, get_value(a, M3)),
    ?assertEqual({ok, 2}, get_value(b, M3)),
    ?assertEqual({ok, 3}, get_value(a, M4)),
    ok.

nested_get_test() ->
    M = vormap:new(),
    M1 = vormap:set([o, a], 1, M),
    M2 = vormap:set([o, b], 2, M1),
    M3 = vormap:set([o, a], 3, M2),
    M4 = vormap:remove([o, b], M3),
    ?assertEqual({ok, 1}, get_value([o, a], M1)),
    ?assertEqual({ok, 1}, get_value([o, a], M2)),
    ?assertEqual({ok, 2}, get_value([o, b], M2)),
    ?assertEqual({ok, 3}, get_value([o, a], M3)),
    ?assertEqual({ok, 2}, get_value([o, b], M3)),
    ?assertEqual({ok, 3}, get_value([o, a], M4)),
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
