
%% Module Under Test 

-module(mut).

-export([
    run/0
        ]).

run() -> 
    fp("#~p: run() ...\n", [?LINE]),
    fun1().

fun1() ->
    fp("#~p: fun1() ...\n", [?LINE]),
    fun2().

fun2() ->
    fp("#~p: fun2() ...\n", [?LINE]),
    mymod:myfun(whatever).

fp(Str) ->
    io:put_chars(Str).

fp(Str, Fmt) ->
    fp(io_lib:format(Str, Fmt)).

