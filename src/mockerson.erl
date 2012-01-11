
-module(mockerson).

%% exports for mocky
-export([
    mock/2,
    call/1
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
            throw({arg_list_mismatch, MisMatchFormat})
    end.

%%% ----------------------------------------------------------------------------
%%% return {ok, Whatever} or {fault, Reason}
%%% ----------------------------------------------------------------------------
make_mocker(Mock) ->
    % {M, F, A} = Mock#mock.mfa,
    %% io:format("mocking ~p:~p/~p~n", [M, F, A]),
    %% io:format("result: ~p\n", [Mock#mock.result]),
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
    FaList = filter_fa_list(lists:sort(get_fa_list(Mod))),
    make_mocker_functions(FaList, Mod, Line).

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
get_fa_list(Mod) ->
    get_fa_list(Mod, get()).

get_fa_list(_Mod, []) ->
    [];
get_fa_list(Mod, [{{Mod, F, A}, _Mock} | Rest]) ->
    [{F, A} | get_fa_list(Rest)];
get_fa_list(Mod, [_Other | Rest]) ->
    get_fa_list(Mod, Rest).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
filter_fa_list([]) ->
    [];
filter_fa_list([A, A | Rest]) ->
    filter_fa_list([A | Rest]);
filter_fa_list([A | Rest]) ->
    [A | filter_fa_list(Rest)].

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
compile_and_load_mocker(AbstractCode) ->
    {ok, Module, Binary} = compile:forms(AbstractCode),
    code:purge(Module),
    code:delete(Module),
    {module, Module} = load_module(Module, Binary).
    %% io:format("mocking module ~p loaded~n", [Module]).

