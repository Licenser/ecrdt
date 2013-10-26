%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%% This implements a simple last write wins register.
%%% @end
%%% Created :  5 Jun 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(vlwwregister).

-behaviour(ecrdt).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0, new/1, assign/2, merge/2, value/1, is_a/1, type/0]).

-record(vlwwregister, {value :: term(),
                       t :: term()}).

-opaque vlwwregister() :: #vlwwregister{}.

-export_type([vlwwregister/0]).

%%%===================================================================
%%% Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tests is the passed data is implementing this type.
%% @end
%%--------------------------------------------------------------------
-spec is_a(any()) -> true | false.

is_a(#vlwwregister{}) ->
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
    register.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new empty LWW register.
%% @end
%%--------------------------------------------------------------------
-spec new(Value::term()) ->
    LWWRegister::vlwwregister().
new(Value) ->
    #vlwwregister{t = now(), value = Value}.

new() ->
    new(undefined).
%%--------------------------------------------------------------------
%% @doc
%% Sets the value of a LWW register and updates the timestamp.
%% @end
%%--------------------------------------------------------------------
-spec assign(Value::term(), LWWRegister::vlwwregister()) ->
    LWWRegister1::vlwwregister().
assign(V, _) ->
    #vlwwregister{value = V, t = now()}.

%%--------------------------------------------------------------------
%% @doc
%% Merges two LWW registers by discarding the older one.
%% @end
%%--------------------------------------------------------------------
-spec merge(LWWRegister1::vlwwregister(),
            LWWRegister2::vlwwregister()) ->
                   LWWRegister1::vlwwregister().
merge(R1 = #vlwwregister{t = T0},
      #vlwwregister{t = T1}) when T0 > T1->
    R1;

merge(_, R2) ->
    R2.

%%--------------------------------------------------------------------
%% @doc
%% Gets the value out of a LWW register.
%% @end
%%--------------------------------------------------------------------
-spec value(LWWRegister::vlwwregister()) -> Value::term().
value(#vlwwregister{value = V}) ->
    V.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

op(a, E, C1, C2, Check) ->
    {assign(E, C1), C2, assign(E, Check)};

op(b, E, C1, C2, Check) ->
    {C1, assign(E, C2), assign(E, Check)};

op(ab, E, C1, C2, Check) ->
    {assign(E, C1), assign(E, C2), assign(E, Check)}.

%% Applies the list of opperaitons to three empty sets.
apply_ops(Ops) ->
    Init = new(0),
    lists:foldl(fun({T, E}, {A, B, C}) ->
                        op(T, E, A, B, C)
                end, {Init, Init, Init}, Ops).

%% A list of opperations and targets.
targets() ->
    list({oneof([a, b, ab]),  pos_integer()}).

prop_mmcounter() ->
    ?FORALL(Ts,  targets(),
            begin
                {A, B, C} = apply_ops(Ts),
                value(C) =:=value(merge(A, B))
            end).

propper_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}, long_result])).
-endif.
