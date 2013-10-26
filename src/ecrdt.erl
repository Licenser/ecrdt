-module(ecrdt).

-export([behaviour_info/1]).

-export([id/0,
         merge/2,
         value/1,
         type/1,
         now_us/0,
         timestamp_us/0]).

%%%===================================================================
%%% Behavioyr
%%%===================================================================

behaviour_info(callbacks) ->
    [{new, 0},
%%     {merge, 2},
     {type, 0},
     {is_a, 1},
     {value, 1}];

behaviour_info(_Other) ->
    undefined.

%%%===================================================================
%%% Implementation
%%%===================================================================

-spec id() -> {term(), atom()}.
id() ->
    {now(), node()}.

merge(A, B)
  when is_tuple(A),
       is_tuple(B) ->
    M = element(1, A),
    M = element(1, B),
    case erlang:function_exported(M, merge, 2) of
        true ->
            M:merge(A, B);
        _ ->
            erlang:error(badarg)
    end.

value(A) when is_tuple(A) ->
    M = element(1, A),
    case erlang:function_exported(M, value, 1) of
        true ->
            M:value(A);
        _ ->
            A
    end;

value(A) ->
    A.

type(A) ->
    M = element(1, A),
    case erlang:function_exported(M, type, 0) of
        true ->
            M:type();
        _ ->
            erlang:error(badarg)
    end.


now_us() ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

timestamp_us() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================
