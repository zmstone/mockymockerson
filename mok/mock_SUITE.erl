
-module(mock_SUITE).

-include("mockymockerson.hrl").

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
all() -> [
    ?ct(ts_arity_result),
    ?ct(ts_args_result),
    ?ct(ts_mocking_fun)
    ].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
ts_arity_result(suite) ->
    [
    ?ct(t_arity_result_normal),
    ?ct(t_arity_result_twice),
    ?ct(t_arity_result_3_times),
    ?ct(t_arity_result_too_many_invokes)
    ].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
t_arity_result_normal(exec) ->
    ?mock(mymod, myfun, {1, ok}),
    ?match(ok, mymod:myfun(whatever)),
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
t_arity_result_twice(exec) ->
    ?mock(mymod, myfun, {1, ok}),
    ?mock(mymod, myfun, {1, cool}),
    ok = mymod:myfun(whatever),
    cool = mymod:myfun(whatsoever),
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
t_arity_result_3_times(exec) ->
    ?mock(mod, func, [{1, ok},
                      {1, cool},
                      {1, good}]),
    ok = mod:func(aaa),
    cool = mod:func(bbb),
    good = mod:func(ccc),
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
t_arity_result_too_many_invokes(exec) ->
    ?mock(mod, func, {0, ok}),
    ok = mod:func(),
    case catch mod:func() of
        ok ->
            exit({failed, t_arity_result_too_many_invokes});
        Exception ->
            ExceptionStr = lists:flatten(io_lib:format("~1000p", [Exception])),
            ?match(true, is_sub_str("Mocker used up", ExceptionStr))
    end,
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
ts_args_result(suite) ->
    [
    ?ct(t_args_result_normal),
    ?ct(t_args_result_batch)
    ].

t_args_result_normal(exec) ->
    ?mock(mymod, myfun, {['_whatever'], ok}),
    ?mock(mymod, myfun, {[must_match], cool}),
    ok = mymod:myfun(whatsoever),
    cool = mymod:myfun(must_match),
    ok.

t_args_result_batch(exec) ->
    ?mock(mymod, myfun, [{[whatever], ok},
                         {[whatsoever], cool},
                         {['_'], {ok, "$don't match this one"}}]),
    ?match(ok, mymod:myfun(whatever)),
    ?match(cool, mymod:myfun(whatsoever)),
    ?match({ok, '_no_match'}, mymod:myfun(crap)),
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
ts_mocking_fun(suite) ->
    [
    ?ct(t_mocking_fun_normal),
    ?ct(t_mocking_fun_undef)
    ].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
t_mocking_fun_normal(exec) ->
    Fun = fun(whatever) -> ok;
             (whatsoever) -> cool;
             (_) -> crash:crash(), {ok, "$don't match this one"}
          end,
    ?mock_n(3, mymod, myfun, Fun),
    ?match(ok, mymod:myfun(whatever)),
    ?match(cool, mymod:myfun(whatsoever)),
    ?match({ok, '_no_match'}, mymod:myfun(crap)),
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
t_mocking_fun_undef(exec) ->
    Fun = fun(whatever) -> ok end,
    ?mock(mymod, myfun, Fun),
    ok = mymod:myfun(whatever),
    try mymod:myfun(whatever, crap) of
        Result ->
            nok
    catch
        error:undef ->
            ok
    end.

%%% ----------------------------------------------------------------------------
%%% INTERNAL HELP FUNCTIONS
%%% ----------------------------------------------------------------------------
is_sub_str([], _Str) ->
    true;
is_sub_str(Sub, Str) when Str == [] orelse length(Sub) > length(Str) ->
    false;
is_sub_str(Sub,  Str) ->
    case lists:prefix(Sub, Str) of
        true ->
            true;
        false ->
            [_ | Rest] = Str,
            is_sub_str(Sub, Rest)
    end.

