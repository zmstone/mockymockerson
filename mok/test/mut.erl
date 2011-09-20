
%% Module Under Test 

-module(mut).

-export([
    run/0
        ]).

run() -> 
    fp("run() ...\n"),
    fun1().

fun1() ->
    fp("fun1() ...\n"),
    fun2().

fun2() ->
    fp("fun2() ...\n"),
    mymod:myfun(whatever).

fp(Str) ->
    io:format(Str).
fp(Str, Fmt) ->
    io:format(Str, Fmt).

