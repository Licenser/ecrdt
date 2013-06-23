-module(ecrdt).

-export([id/0,
         merge/2,
         value/1]).

%%%===================================================================
%%% Implementation
%%%===================================================================

-spec id() -> {term(), atom()}.
id() ->
    {now(), node()}.

merge(A, B) ->
    M = element(1, A),
    M = element(1, B),
    case erlang:function_exported(M, merge, 2) of
        true ->
            M:merge(A, B);
        _ ->
            erlang:error(badarg)
    end.

value(A) when is_tupel(A) ->
    M = element(1, A),
    case erlang:function_exported(M, value, 1) of
        true ->
            M:value(A);
        _ ->
            A
    end;

value(A) ->
    A.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================
