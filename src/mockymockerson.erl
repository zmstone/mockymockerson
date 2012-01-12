
-module(mockymockerson).

-export([
     mock/5
    ,mock_n/6
    ,call/3
    ,start/0
    ,stop/0
]).

-include("mockymockerson_private.hrl").

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
start() -> mocky:start().

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
stop() ->
    case mocky:purge() of
    {?exception, Exception} ->
        mocky:stop(),
        throw(Exception);
    _ ->
        mocky:stop()
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
mock(TestMod, Line, Module, Function, ArgRstList)
  when is_list(ArgRstList) andalso length(ArgRstList) > 0 ->
    lists:foreach(fun(Whatever) ->
                        mock(TestMod, Line, Module, Function, Whatever)
                  end, ArgRstList);

mock(TestMod, Line, Module, Function, {Arity, Result})
  when is_integer(Arity) ->
    Args = lists:duplicate(Arity, '_'),
    mock(TestMod, Line, Module, Function, {Args, Result});

mock(TestMod, Line, Module, Function, {Args, Result})
  when is_list(Args) ->
    Mocker = #mock{ mfa     = {Module, Function, length(Args)},
                    tester  = TestMod,
                    line    = Line,
                    expArgs = Args,
                    result  = Result },
    mock(Mocker);

mock(TestMod, Line, Module, Function, MockerFun)
  when is_function(MockerFun) ->
    {arity, Arity} = erlang:fun_info(MockerFun, arity),
    Mocker = #mock{ mfa     = {Module, Function, Arity},
                    tester  = TestMod,
                    line    = Line,
                    mocker  = MockerFun },
    mock(Mocker);

mock(_TestMod, _Line, _Module, _Function, Whatever) ->
    throw({bad_mocking_arg, Whatever}).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
mock_n(0, _TestMod, _Line, _Module, _Function, _Whatever) ->
    ok;
mock_n(N, TestMod, Line, Module, Function, Whatever) ->
    mock(TestMod, Line, Module, Function, Whatever),
    mock_n(N-1, TestMod, Line, Module, Function, Whatever).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
mock(#mock{mfa = {Module, _F, _A}} = Mock) ->
    case code:which(Module) of
        non_existing ->
            mock_help(Mock);
        _ ->
            throw("Can not mock loaded module")
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
mock_help(Mock) ->
    catch mocky:start(),
    case gen_server:call(?SERVER, Mock) of
        ok ->
            ok;
        {fault, Reason} ->
            mocky:stop(),
            throw(Reason)
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
call(Module, Function, ArgList) ->
    MockCall = #mock_call{mfa = {Module, Function, length(ArgList)},
                          realArgs = ArgList},
    %% mok_break:pause(MockCall),
    case gen_server:call(?SERVER, MockCall) of
        {?exception, Exception} ->
            throw(Exception);
        ReturnValue ->
            ReturnValue
    end.
