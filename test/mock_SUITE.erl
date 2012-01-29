
-module(mock_SUITE).

%% CT hooks
-export([
     init_per_testcase/2
    ,end_per_testcase/2
]).

-include("mockymockerson.hrl").

%% test cases
-export(
    [ all/0

    , t_arity_result_normal/1
    , t_arity_result_twice/1
    , t_arity_result_3_times/1
    , t_arity_result_too_many_invokes/1
    , t_mock_two_different_modules/1

    , t_args_result_normal/1
    , t_args_result_batch/1

    , t_mocking_fun_normal/1
    , t_mocking_fun_undef/1

    , t_extra_mocked_functions/1
    , t_mock_mut_run/1
    , t_mock_loaded_module/1
    , t_order_matters/1
    ]).

%%% ----------------------------------------------------------------------------
%%% CT hook
%%% ----------------------------------------------------------------------------
init_per_testcase(_Tc, Config) ->
    mockymockerson:setup(),
    Config.

%%% ----------------------------------------------------------------------------
%%% CT hook
%%% ----------------------------------------------------------------------------
end_per_testcase(_Tc, _Config) ->
    mockymockerson:clear().

%%% ----------------------------------------------------------------------------
%%% All tests
%%% ----------------------------------------------------------------------------
all() ->
    [ t_arity_result_normal
    , t_arity_result_twice
    , t_arity_result_3_times
    , t_arity_result_too_many_invokes
    , t_mock_two_different_modules
    , t_args_result_normal
    , t_args_result_batch
    , t_mocking_fun_normal
    , t_mocking_fun_undef
    , t_extra_mocked_functions
    , t_mock_mut_run
    , t_mock_loaded_module
    , t_order_matters
    ].

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun once
%%% ----------------------------------------------------------------------------
t_arity_result_normal(Conf) when is_list(Conf) ->
    ?mock(mymod, myfun, {1, ok}),
    ?assertMatch(ok, mymod:myfun(whatever)).

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun twice
%%% ----------------------------------------------------------------------------
t_arity_result_twice(Conf) when is_list(Conf) ->
    ?mock(mymod, myfun, {1, ok}),
    ?mock(mymod, myfun, {1, cool}),
    ok = mymod:myfun(whatever),
    cool = mymod:myfun(whatsoever).

%%% ----------------------------------------------------------------------------
%%% Mock mod:fun by a given list of {arity result}
%%% ----------------------------------------------------------------------------
t_arity_result_3_times(Conf) when is_list(Conf) ->
    ?mock(mod, func, [{1, ok},
                      {1, cool},
                      {1, good}]),
    ok = mod:func(aaa),
    cool = mod:func(bbb),
    good = mod:func(ccc).

%%% ----------------------------------------------------------------------------
%%% mod:fun is invoked more times than it is mocked
%%% ----------------------------------------------------------------------------
t_arity_result_too_many_invokes(Conf) when is_list(Conf) ->
    ?mock(mod, func, {1, ok}),
    ok = mod:func(a),
    case catch mod:func(a) of
        ok ->
            throw(ailed);
        Exception ->
            ExceptionStr = lists:flatten(io_lib:format("~1000p", [Exception])),
            ?assertMatch(true, is_sub_str("Mocker used up", ExceptionStr))
    end.

%%% ----------------------------------------------------------------------------
%%% mock two different modules
%%% ----------------------------------------------------------------------------
t_mock_two_different_modules(Conf) when is_list(Conf) ->
    ?mock(mymod1, myfun, {0, ok}),
    ?mock(mymod2, myfun, {1, ok}),
    ok = mymod1:myfun(),
    ok = mymod2:myfun(a).


%%% ----------------------------------------------------------------------------
%%% mock mod:fun one time in one line
%%% ----------------------------------------------------------------------------
t_args_result_normal(Conf) when is_list(Conf) ->
    ?mock(mymod, myfun, {['_'], ok}),
    ?mock(mymod, myfun, {[must_match], cool}),
    ok = mymod:myfun(whatsoever),
    cool = mymod:myfun(must_match).

%%% ----------------------------------------------------------------------------
%%% mock mod:fun by given list of arg-list and result
%%% ----------------------------------------------------------------------------
t_args_result_batch(Conf) when is_list(Conf) ->
    ?mock(mymod, myfun, [{[whatever], ok},
                         {[whatsoever], cool},
                         {['_'], {ok, "$don't match this one"}}]),
    ?assertMatch(ok, mymod:myfun(whatever)),
    ?assertMatch(cool, mymod:myfun(whatsoever)),
    ?assertMatch({ok, '_'}, mymod:myfun(crap)).

%%% ----------------------------------------------------------------------------
%%% mock mod:fun by given function with specific number of calls
%%% ----------------------------------------------------------------------------
t_mocking_fun_normal(Conf) when is_list(Conf) ->
    Fun = fun(whatever) -> ok;
             (whatsoever) -> cool;
             (_) -> {ok, "$don't match this one"}
          end,
    ?mock_n(3, mymod, myfun, Fun),
    ?assertMatch(ok, mymod:myfun(whatever)),
    ?assertMatch(cool, mymod:myfun(whatsoever)),
    {ok, _} = mymod:myfun(crap).

%%% ----------------------------------------------------------------------------
%%% mock mod:fun/1 but no mod:fun/2
%%% ----------------------------------------------------------------------------
t_mocking_fun_undef(Conf) when is_list(Conf) ->
    Fun = fun(whatever) -> ok end,
    ?mock(mymod, myfun, Fun),
    ok = mymod:myfun(whatever),
    try mymod:myfun(whatever, crap) of
    _Result ->
       throw(failed)
    catch
    error:undef ->
       ok
    end.

%%% ----------------------------------------------------------------------------
%%% mocked too many
%%% ----------------------------------------------------------------------------
t_extra_mocked_functions(Conf) when is_list(Conf) ->
    ?mock(mymod, myfun, {0, ok}),
    try mockymockerson:clear() of
    _Result ->
        throw(failed)
    catch
    throw:{"Mocked function(s) not called",
           [{mymod, myfun, 0}]} ->
        ok
    end.

%%% ----------------------------------------------------------------------------
%%% Test mut:run/0
%%% ----------------------------------------------------------------------------
t_mock_mut_run(Conf) when is_list(Conf) ->
    ?mock(mymod, myfun, {[whatever], ok}),
    ok = mut:run().

%%% ----------------------------------------------------------------------------
%%% should not mock a loaded module, because it's supposed to be an test obj
%%% ----------------------------------------------------------------------------
t_mock_loaded_module(Conf) when is_list(Conf) ->
    try ?mock(mut, run, {0, ok}) of
    _ ->
        throw(failed)
    catch
    throw:Exception ->
        ?assertEqual({"Can not mock loaded module", {mut, run, 0}}, Exception)
    end.

%%% ----------------------------------------------------------------------------
%%% the order of mocking a function multiple times matters
%%% ----------------------------------------------------------------------------
t_order_matters(Conf) when is_list(Conf) ->
    ?mock(mod, func, {[matters2], matters2}),
    ?mock(mod, func, {[matters1], matters1}),
    try mod:func(matters1) of
    _ ->
        throw(failed)
    catch
    throw:{"Arg list mismatch", _} ->
        ok
    end,
    matters2 = mod:func(matters2),
    matters1 = mod:func(matters1).

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

