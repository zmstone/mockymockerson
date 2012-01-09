
-module(mock_SUITE).

-export([all_test/0]).

-include("mockymockerson.hrl").

%%% ----------------------------------------------------------------------------
%%% All test suites and cases
%%% ----------------------------------------------------------------------------
all_test() -> 
    ts_arity_result(),
    ts_args_result(),
    ts_mocking_fun(),
    ts_mocking_error(),
    t_mut_run(),
    ok.

%%% ----------------------------------------------------------------------------
%%% Mock functions with arity and result
%%% ----------------------------------------------------------------------------
ts_arity_result() ->
    t_arity_result_normal(),
    t_arity_result_twice(),
    t_arity_result_3_times(),
    t_arity_result_too_many_invokes(),
    ok.

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun once
%%% ----------------------------------------------------------------------------
t_arity_result_normal() ->
    ?mock(mymod, myfun, {1, ok}),
    ?match(ok, mymod:myfun(whatever)),
    ok.

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun twice
%%% ----------------------------------------------------------------------------
t_arity_result_twice() ->
    ?mock(mymod, myfun, {1, ok}),
    ?mock(mymod, myfun, {1, cool}),
    ok = mymod:myfun(whatever),
    cool = mymod:myfun(whatsoever),
    ok.

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun by a given list of {arity result}
%%% ----------------------------------------------------------------------------
t_arity_result_3_times() ->
    ?mock(mod, func, [{1, ok},
                      {1, cool},
                      {1, good}]),
    ok = mod:func(aaa),
    cool = mod:func(bbb),
    good = mod:func(ccc),
    ok.

%%% ----------------------------------------------------------------------------
%%% mod:fun is invoked more times than it is mocked
%%% ----------------------------------------------------------------------------
t_arity_result_too_many_invokes() ->
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
%%% mock mod:fun by given arg list and result
%%% ----------------------------------------------------------------------------
ts_args_result() ->
    t_args_result_normal(),
    t_args_result_batch(),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun one time in one line
%%% ----------------------------------------------------------------------------
t_args_result_normal() ->
    ?mock(mymod, myfun, {['_whatever'], ok}),
    ?mock(mymod, myfun, {[must_match], cool}),
    ok = mymod:myfun(whatsoever),
    cool = mymod:myfun(must_match),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun by given list of arg-list and result
%%% ----------------------------------------------------------------------------
t_args_result_batch() ->
    ?mock(mymod, myfun, [{[whatever], ok},
                         {[whatsoever], cool},
                         {['_'], {ok, "$don't match this one"}}]),
    ?match(ok, mymod:myfun(whatever)),
    ?match(cool, mymod:myfun(whatsoever)),
    ?match({ok, '_no_match'}, mymod:myfun(crap)),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun by given function
%%% ----------------------------------------------------------------------------
ts_mocking_fun() ->
    t_mocking_fun_normal(),
    t_mocking_fun_undef(),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun by given function with specific number of calls
%%% ----------------------------------------------------------------------------
t_mocking_fun_normal() ->
    Fun = fun(whatever) -> ok;
             (whatsoever) -> cool;
             (_) -> {ok, "$don't match this one"}
          end,
    ?mock_n(3, mymod, myfun, Fun),
    ?match(ok, mymod:myfun(whatever)),
    ?match(cool, mymod:myfun(whatsoever)),
    ?match({ok, '_no_match'}, mymod:myfun(crap)),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun/1 but no mod:fun/2
%%% ----------------------------------------------------------------------------
t_mocking_fun_undef() ->
    Fun = fun(whatever) -> ok end,
    ?mock(mymod, myfun, Fun),
    ok = mymod:myfun(whatever),
    try mymod:myfun(whatever, crap) of
        _Result ->
            nok
    catch
        error:undef ->
            ok
    end.

%%% ----------------------------------------------------------------------------
%%% when trying to mock a loaded module
%%% ----------------------------------------------------------------------------
ts_mocking_error() ->
    t_mocking_error_loaded_module(),
    ok.

%%% ----------------------------------------------------------------------------
%%% when trying to mock a loaded module
%%% ----------------------------------------------------------------------------
t_mocking_error_loaded_module() ->
    try ?mock(mockymockerson, whatever_function, whatever_arity) of
        _Result ->
            nok
    catch
        throw:_Reason ->
            ok
    end.

%%% ----------------------------------------------------------------------------
%%% Test mut:run/0
%%% ----------------------------------------------------------------------------
t_mut_run() ->
    ?mock(mymod, myfun, {[whatever], ok}),
    ok = mut:run(),
    ok.

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

