
-module(mock_test).

-compile(export_all).

-include("mockymockerson.hrl").

-define(match(A, B), mymatch:run(?MODULE, ?LINE, A, B)).

run() ->
    run(all()).

run([]) ->
    ok;
run([CaseFun | Rest]) ->
    case catch CaseFun(suite) of
        SubCaseList when is_list(SubCaseList) andalso SubCaseList /= [] ->
            run(SubCaseList);
        _ ->
            exec(CaseFun)
    end,
    run(Rest).

exec(CaseFun) ->
    mockymockerson:start(),
    io:format("running test: ~p~n", [CaseFun]),
    CaseFun(exec),
    mockymockerson:stop().

all() -> [
    fun test_arity_result/1,
    fun test_args_result/1
    ].

test_arity_result(suite) ->
    [
    fun test_arity_result_normal/1,
    fun test_arity_result_twice/1,
    fun test_arity_result_3_times/1,
    fun test_arity_result_too_many_invoke/1
    ].

test_arity_result_normal(exec) ->
    ?mock(mymod, myfun, {1, ok}),
    ?match(ok, mymod:myfun(whatever)).

test_arity_result_twice(exec) ->
    ?mock(mymod, myfun, {1, ok}),
    ?mock(mymod, myfun, {1, cool}),
    ok = mymod:myfun(whatever),
    cool = mymod:myfun(whatsoever).

test_arity_result_3_times(exec) ->
    ?mock(mod, func, [{1, ok},
                      {1, cool},
                      {1, good}]),
    ok = mod:func(aaa),
    cool = mod:func(bbb),
    good = mod:func(ccc).

test_arity_result_too_many_invoke(exec) ->
    ?mock(mod, func, {0, ok}),
    ok = mod:func(),
    case catch mod:func() of
        ok ->
            exit({failed, test_arity_result_too_many_invoke});
        Exception ->
            ExceptionStr = lists:flatten(io_lib:format("~100000p", [Exception])),
            ?match(true, is_sub_str("Mocker used up", ExceptionStr))
    end.

test_args_result(exec) ->
    ?mock(mymod, myfun, {['_whatever'], ok}),
    ?mock(mymod, myfun, {[must_match], cool}),
    ok = mymod:myfun(whatsoever),
    cool = mymod:myfun(must_match).

%%% ----------------------------------------------------------------------------
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

