-module(ecrdt).

-export([id/0,
         merge/2,
         value/1,
         now_us/0,
         timestamp_us/0]).

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

now_us() ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
	{(MegaSecs*1000000 + Secs)*1000000 + MicroSecs, test}.

timestamp_us() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
	{(MegaSecs*1000000 + Secs)*1000000 + MicroSecs, test}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================
