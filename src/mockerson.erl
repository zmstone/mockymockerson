
%%%
%%% mockerson the slave
%%%

-module(mockerson).

%% exports for mocky
-export([
     mock/2
    ,call/2
    ,clear/1
]).

-include("mockymockerson_private.hrl").

-define(mocked_mod_entry, mockymockerson).
-define(mocked_fun_entry, call).

take_first(_Mfa, [], _Acc) ->
    ?undef;
take_first(Mfa, [Mock | Rest], Acc) ->
    case Mock of
    #mock{mfa = Mfa} ->
        {Mock, lists:reverse(Acc) ++ Rest};
    _ ->
        take_first(Mfa, Rest, [Mock | Acc])
    end.

unique_mfa_list(#mocker_state{used_list = UsedList,
                              mock_list = MockList}) ->
    unique_mfa_list(UsedList ++ MockList);
unique_mfa_list(MockList) ->
    lists:usort([Mfa || #mock{mfa = Mfa} <- MockList]).

%%% ----------------------------------------------------------------------------
%%% add a new mock, fake a function and compile the module then re-load it
%%% if it's a new function. i.e. never mocked before
%%% ----------------------------------------------------------------------------
mock(#mock{mfa = Mfa} = Mock,
     #mocker_state{mock_list = MockList} = State) ->
    MfaList = unique_mfa_list(State),
    case lists:member(Mfa, MfaList) of
    true ->
        %% this function is already mocked befre
        do_nothing;
    false ->
        %% this function is not mocked yet
        make_mocker([Mfa | MfaList])
    end,
    {ok, State#mocker_state{mock_list = MockList ++ [Mock]}}.

%%% ----------------------------------------------------------------------------
%%% Handle a call from the fake mocked function
%%% return the return value for the mocked function
%%% ----------------------------------------------------------------------------
call(#call{mfa = Mfa, realArgs = RealArgs},
     #mocker_state{used_list = UsedList, mock_list = MockList} = State) ->
    MfaList = unique_mfa_list(MockList),
    case lists:member(Mfa, MfaList) of
    true ->
        {Mock, NewMockList} = take_first(Mfa, MockList, []),
        {call_mocker(Mock, RealArgs),
         State#mocker_state{used_list = [Mock | UsedList],
                            mock_list = NewMockList}};
    false ->
        throw(?excep({"Mocker used up", Mfa}))
    end.

%%% ----------------------------------------------------------------------------
%%% clean up the mockers, purge the loaded fake module
%%% ----------------------------------------------------------------------------
clear(#mocker_state{used_list = UsedList, mock_list = MockList} = State) ->
    case UsedList ++ MockList of
    [] ->
        do_nothing;
    [#mock{mfa = {Module, _F, _A}} | _] ->
        purge(Module)
    end,
    {unique_mfa_list(MockList), State}.

%%% ============================================================================
%%% INTERNAL FUNCTIONS
%%% ============================================================================

%%% ----------------------------------------------------------------------------
%%% return the return value for the mocked function
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
%%% fake a module with the mocked functions
%%% ----------------------------------------------------------------------------
make_mocker(MfaList) ->
    AbstractCode = make_mocker_attributes(MfaList) ++
                   make_mocker_functions(MfaList),
    compile_and_load_mocker(AbstractCode),
    {ok, mocked}.

%%% ----------------------------------------------------------------------------
%%% make the fake module export all functions
%%% ----------------------------------------------------------------------------
make_mocker_attributes([{Mod, _F, _A} | _]) ->
    [{attribute, 1, module, Mod},
     {attribute, 1, compile, export_all}].

%%% ----------------------------------------------------------------------------
%%% make the mocked function
%%% ----------------------------------------------------------------------------
make_mocker_functions(MfaList) ->
    {Mod, _F, _A} = hd(MfaList),
    FaList = [{F, A} || {_M, F, A} <- MfaList],
    make_mocker_functions(FaList, Mod).

make_mocker_functions([], _Mod) ->
    [];
make_mocker_functions([{Fun, Arity} | FaList], Mod) ->
    ArgList = make_mocker_arg_list(0, Arity),
    Function =
        {function, 1, Fun, Arity,
            [{clause, 1,
                ArgList,
                [],
                [{call, 1,
                    {remote, 1,
                     {atom, 1, ?mocked_mod_entry},
                     {atom, 1, ?mocked_fun_entry}},
                    [{atom, 1, Mod},
                     {atom, 1, Fun},
                     make_cons_arg_tuple(ArgList)
                    ]
                 }]
            }]
        },
    [Function | make_mocker_functions(FaList, Mod)].

%%% ----------------------------------------------------------------------------
%%% make the argument list for the mocked function 
%%% ----------------------------------------------------------------------------
make_mocker_arg_list(N, N) ->
    [];
make_mocker_arg_list(I, N) ->
    Var = {var, 1, list_to_atom("A" ++ integer_to_list(I))},
    [Var | make_mocker_arg_list(I+1, N)].

%%% ----------------------------------------------------------------------------
%%% represent list as 'cons' tuples
%%% ----------------------------------------------------------------------------
make_cons_arg_tuple([]) ->
    {nil, 1};
make_cons_arg_tuple([Arg | ArgList]) ->
    {cons, 1, Arg, make_cons_arg_tuple(ArgList)}.

%%% ----------------------------------------------------------------------------
%%% compile and load the fake module
%%% ----------------------------------------------------------------------------
compile_and_load_mocker(AbstractCode) ->
    {ok, Module, Binary} = compile:forms(AbstractCode),
    purge(Module),
    %% ?fp("loading ~p\n", [AbstractCode]),
    {module, Module} = load_module(Module, Binary).

%%% ----------------------------------------------------------------------------
%%% delete the fake module from runtime
%%% ----------------------------------------------------------------------------
purge(Module) ->
    code:purge(Module),
    code:delete(Module).

