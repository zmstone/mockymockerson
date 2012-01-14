
%%%
%%% mockymockerson the application
%%%

-module(mockymockerson).

%% external calls
-export([
     start/0
    ,stop/0
    ,setup/0
    ,clear/0
    ,mock/5
    ,mock_n/6
    ,call/3
]).

%% application callbacks
-export([
     start/2
    ,stop/1
]).

-include("mockymockerson_private.hrl").

%%% ----------------------------------------------------------------------------
%%% external call to start application
%%% ----------------------------------------------------------------------------
start() ->
    catch application:start(eunit),
    catch application:start(mockymockerson).

%%% ----------------------------------------------------------------------------
%%% application start callback
%%% ----------------------------------------------------------------------------
start(_Type, Args) ->
    mockymockerson_sup:start(Args).

%%% ----------------------------------------------------------------------------
%%% external call to stop application
%%% ----------------------------------------------------------------------------
stop() ->
    application:stop(?SERVER).

%%% ----------------------------------------------------------------------------
%%% application stop callback
%%% ----------------------------------------------------------------------------
stop(_State) ->
    %% io:format("mockymockerson stoped\n"),
    ok.

%%% ----------------------------------------------------------------------------
%%% setup the mocky-testing env
%%% ----------------------------------------------------------------------------
setup() ->
    %% silent clean up
    catch clear().

%%% ----------------------------------------------------------------------------
%%% clean up the mocky-testing env
%%% ----------------------------------------------------------------------------
clear() ->
    case mocky:purge() of
    {?exception, Exception} ->
        throw(Exception);
    _ ->
        ok
    end.

%%% ----------------------------------------------------------------------------
%%% mock a single call
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
%%% mock in a batch
%%% ----------------------------------------------------------------------------
mock_n(0, _TestMod, _Line, _Module, _Function, _Whatever) ->
    ok;
mock_n(N, TestMod, Line, Module, Function, Whatever) ->
    mock(TestMod, Line, Module, Function, Whatever),
    mock_n(N-1, TestMod, Line, Module, Function, Whatever).

%%% ----------------------------------------------------------------------------
%%% mock a m:f/a call
%%% ----------------------------------------------------------------------------
mock(#mock{mfa = {Module, _F, _A}} = Mock) ->
    case code:which(Module) of
        non_existing ->
            mockymockerson_sup:dispatch(Mock);
        _ ->
            throw("Can not mock loaded module")
    end.

%%% ----------------------------------------------------------------------------
%%% This is the callback function provided to the fake modules
%%% e.g. if there is a mod:func/0 mocked, the fake code actually looks
%%%      like this mod:func() -> mockymockerson:call(mod, fun, [])
%%% ----------------------------------------------------------------------------
call(Module, Function, ArgList) ->
    MockCall = #mock_call{mfa = {Module, Function, length(ArgList)},
                          realArgs = ArgList},
    mockymockerson_sup:dispatch(MockCall).

