
%%%
%%% mockerson works for mocky the server
%%%

-module(mockerson).

%% exports for mocky
-export([
     mock/2
    ,call/1
    ,purge/0
]).

-include("mockymockerson_private.hrl").

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
mock(Mockerson, #mock{mfa = Mfa} = Mock) ->
    case get(Mfa) of
        MockList when is_list(MockList) ->
            put(Mfa, MockList ++ [Mock]),
            {ok, Mockerson};
        _ ->
            put(Mfa, [Mock]),
            make_mocker(Mock)
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
purge() ->
    AllMockInfo = get_all_mock_info(),
    AllModules = [M || {{M, _F, _A}, _MockList} <- AllMockInfo],
    [purge(Module) || Module <- AllModules],
    ExtraMocks = [{M, F, A, length(MockList)} ||
                  {{M, F, A}, MockList} <- AllMockInfo, MockList /= []],
    case ExtraMocks of
    [] ->
        ok;
    _ ->
        throw(?excep({"Mocked function not called", ExtraMocks}))
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
call(#mock_call{mfa = Mfa, realArgs = RealArgs}) ->
    case get(Mfa) of
        [] ->
            throw(?excep({"Mocker used up", Mfa}));
        [Mock | MockList] ->
            put(Mfa, MockList),
            call_mocker(Mock, RealArgs);
        _ ->
            throw(?excep({"Mocker undefined", Mfa}))
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
call_mocker(#mock{mocker = Mocker}, RealArgs) when is_function(Mocker) ->
    case catch apply(Mocker, RealArgs) of
        {'EXIT', Reason} ->
            throw(?excep({"Mocker function crashed", Reason}));
        Result ->
            Result
    end;
call_mocker(#mock{tester = Mod,
                  expArgs = ExpArgs,
                  line = Line} = Mock, RealArgs) when is_list(ExpArgs) ->
    FixedArgs = mockymockerson_ignore:fix(ExpArgs, RealArgs),
    case catch mockymockerson_match:run(Mod, Line, FixedArgs, RealArgs, []) of
        ok ->
            Mock#mock.result;
        {_, MisMatchFormat} ->
            throw(?excep({"Arg List mismatch", MisMatchFormat}))
    end.

%%% ----------------------------------------------------------------------------
%%% return {ok, Whatever} or {fault, Reason}
%%% ----------------------------------------------------------------------------
make_mocker(Mock) ->
    %% {M, F, A} = Mock#mock.mfa,
    %% ?fp("mocking ~p:~p/~p~n", [M, F, A]),
    %% ?fp("result: ~p\n", [Mock#mock.result]),
    AbstractCode = make_mocker_attributes(Mock) ++
                   make_mocker_functions(Mock),
    compile_and_load_mocker(AbstractCode),
    {ok, mocked}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
make_mocker_attributes(#mock{mfa  = {Mod, _Fun, _Arity},
                             line = Line}) ->
    [{attribute, Line, module, Mod},
     {attribute, Line, compile, export_all}].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
make_mocker_functions(#mock{mfa  = {Mod, _Fun, _Arity},
                            line = Line}) ->
    AllMockInfo = get_all_mock_info(),
    FaList0 = [{F, A} || {{M, F, A}, _Mockers} <- AllMockInfo, M == Mod],
    FaList1 = lists:usort(FaList0),
    make_mocker_functions(FaList1, Mod, Line).

make_mocker_functions([], _Mod, _Line) ->
    [];
make_mocker_functions([{Fun, Arity} | FaList], Mod, Line) ->
    ArgList = make_mocker_arg_list(0, Arity, Line),
    Function =
        {function, Line, Fun, Arity,
            [{clause, Line,
                ArgList,
                [],
                [{call, Line,
                    {remote, Line, {atom, Line, ?SERVER}, {atom, Line, call}},
                    [{atom, Line, Mod},
                     {atom, Line, Fun},
                     make_cons_arg_tuple(ArgList, Line)
                    ]
                 }]
            }]
        },
    [Function | make_mocker_functions(FaList, Mod, Line)].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
make_mocker_arg_list(N, N, _Line) ->
    [];
make_mocker_arg_list(I, N, Line) ->
    Var = {var, Line, list_to_atom("A" ++ integer_to_list(I))},
    [Var | make_mocker_arg_list(I+1, N, Line)].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
make_cons_arg_tuple([], Line) ->
    {nil, Line};
make_cons_arg_tuple([Arg | ArgList], Line) ->
    {cons, Line, Arg, make_cons_arg_tuple(ArgList, Line)}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
get_all_mock_info() ->
    [M || M = {{_M, _F, _A}, _MockList} <- get()].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
compile_and_load_mocker(AbstractCode) ->
    {ok, Module, Binary} = compile:forms(AbstractCode),
    purge(Module),
    {module, Module} = load_module(Module, Binary).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
purge(Module) ->
    code:purge(Module),
    code:delete(Module).

