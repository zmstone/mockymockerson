
-module(mock_SUITE).

-include("mockymockerson.hrl").

-export([
]).

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun once
%%% ----------------------------------------------------------------------------
arity_result_normal_test() ->
    ?mock(mymod, myfun, {1, ok}),
    ?match(ok, mymod:myfun(whatever)),
    ok.

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun twice
%%% ----------------------------------------------------------------------------
arity_result_twice_test() ->
    ?mock(mymod, myfun, {1, ok}),
    ?mock(mymod, myfun, {1, cool}),
    ok = mymod:myfun(whatever),
    cool = mymod:myfun(whatsoever),
    ok.

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun by a given list of {arity result}
%%% ----------------------------------------------------------------------------
arity_result_3_times_test() ->
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
arity_result_too_many_invokes_test() ->
    ?mock(mod, func, {1, ok}),
    ok = mod:func(a),
    case catch mod:func(a) of
        ok ->
            exit({failed, t_arity_result_too_many_invokes});
        Exception ->
            ExceptionStr = lists:flatten(io_lib:format("~1000p", [Exception])),
            ?match(true, is_sub_str("Mocker used up", ExceptionStr))
    end,
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun one time in one line
%%% ----------------------------------------------------------------------------
args_result_normal_test() ->
    ?mock(mymod, myfun, {['_whatever'], ok}),
    ?mock(mymod, myfun, {[must_match], cool}),
    ok = mymod:myfun(whatsoever),
    cool = mymod:myfun(must_match),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun by given list of arg-list and result
%%% ----------------------------------------------------------------------------
args_result_batch_test() ->
    ?mock(mymod, myfun, [{[whatever], ok},
                         {[whatsoever], cool},
                         {['_'], {ok, "$don't match this one"}}]),
    ?match(ok, mymod:myfun(whatever)),
    ?match(cool, mymod:myfun(whatsoever)),
    ?fixed_match({ok, '_no_match'}, mymod:myfun(crap)),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun by given function with specific number of calls
%%% ----------------------------------------------------------------------------
mocking_fun_normal_test() ->
    Fun = fun(whatever) -> ok;
             (whatsoever) -> cool;
             (_) -> {ok, "$don't match this one"}
          end,
    ?mock_n(3, mymod, myfun, Fun),
    ?match(ok, mymod:myfun(whatever)),
    ?match(cool, mymod:myfun(whatsoever)),
    {ok, _} = mymod:myfun(crap),
    ok.

%%% ----------------------------------------------------------------------------
%%% mock mod:fun/1 but no mod:fun/2
%%% ----------------------------------------------------------------------------
mocking_fun_undef_test() ->
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
mocking_error_loaded_module_test() ->
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
mut_run_test() ->
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

