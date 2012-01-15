
%%%
%%% mockymockerson the application
%%%

-module(mockymockerson).

%% external calls
-export([
     setup/0
    ,clear/0
    ,mock/5
    ,mock_n/6
    ,exec/3
]).

%% application callbacks
-export([
     start/2
]).

-include("mockymockerson_private.hrl").

%%% ----------------------------------------------------------------------------
%%% application start callback
%%% ----------------------------------------------------------------------------
start(_Type, Args) ->
    mockymockerson_sup:start(Args).

%%% ----------------------------------------------------------------------------
%%% setup the mocky-testing env
%%% ----------------------------------------------------------------------------
setup() ->
    catch application:start(?MODULE),
    %% silent clean up
    catch clear(),
    ok.

%%% ----------------------------------------------------------------------------
%%% clean up the mocky-testing env
%%% ----------------------------------------------------------------------------
clear() ->
    case mockymockerson_sup:clear() of
    [] ->
        ok;
    ExtraMocks ->
        throw({"Mocked function(s) not called", ExtraMocks})
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
    throw({"Bad mocking args", Whatever}).

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
mock(#mock{mfa = {Module, F, A}} = Mock) ->
    case code:which(Module) of
        non_existing ->
            mockymockerson_sup:dispatch(Mock);
        _ ->
            throw({"Can not mock loaded module", {Module, F, A}})
    end.

%%% ----------------------------------------------------------------------------
%%% This is the callback function provided to the fake modules
%%% e.g. if there is a mod:func/0 mocked, the fake code actually looks
%%%      like this mod:func() -> mockymockerson:exec(mod, fun, [])
%%% ----------------------------------------------------------------------------
exec(Module, Function, ArgList) ->
    Exec = #exec{mfa = {Module, Function, length(ArgList)},
                 realArgs = ArgList},
    mockymockerson_sup:dispatch(Exec).

