
%%%
%%% mockerson do dirty jobs for mocker
%%%

-module(mockerson).

%% exports for mocky
-export([
     mock/2
    ,exec/2
    ,clear/1
]).

-include("mockymockerson_private.hrl").

-define(mocked_mod_entry, mockymockerson).
-define(mocked_fun_entry, exec).

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
%%% return the registered return value or the mock-fun to be evaluated
%%% ----------------------------------------------------------------------------
exec(#exec{mfa = Mfa, realArgs = RealArgs},
     #mocker_state{mock_list = MockList} = State) ->
    MfaList = unique_mfa_list(MockList),
    case lists:member(Mfa, MfaList) of
        true ->
            do_exec(Mfa, RealArgs, State);
        false ->
            throw(?excep({"Mocker used up", Mfa}))
    end.

%%% ----------------------------------------------------------------------------
%%% help function of exec
%%% ----------------------------------------------------------------------------
do_exec(Mfa, RealArgs, #mocker_state{used_list = UsedList,
                                     mock_list = MockList} = State) ->
    {Mock, NewMockList} = take_first(Mfa, MockList, []),
    case call_mocker(Mock, RealArgs) of
        {ok, Result} ->
            {Result, State#mocker_state{used_list = [Mock | UsedList],
                                        mock_list = NewMockList}};
        {nok, Reason} ->
            throw(?excep(Reason))
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
    {ok, {?evaluate, fun() -> apply(Mocker, RealArgs) end}};
call_mocker(#mock{tester = Mod,
                  expArgs = ExpArgs,
                  line = Line} = Mock, RealArgs) when is_list(ExpArgs) ->
    FixedArgs = erlymatch_ignore:fix(ExpArgs, RealArgs),
    case catch erlymatch:run(Mod, Line, FixedArgs, RealArgs, []) of
        ok ->
            {ok, Mock#mock.result};
        {_, MisMatchFormat} ->
            {nok, {"Arg list mismatch", MisMatchFormat}}
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
    {module, Module} = load_module(Module, Binary).

%%% ----------------------------------------------------------------------------
%%% delete the fake module from runtime
%%% ----------------------------------------------------------------------------
purge(Module) ->
    code:purge(Module),
    code:delete(Module).

%%% ----------------------------------------------------------------------------
%%% Take the first Mfa match from mock list and return the rest of the mockers
%%% ----------------------------------------------------------------------------
take_first(Mfa, [Mock | Rest], Acc) ->
    case Mock of
    #mock{mfa = Mfa} ->
        {Mock, lists:reverse(Acc) ++ Rest};
    _ ->
        take_first(Mfa, Rest, [Mock | Acc])
    end.

%%% ----------------------------------------------------------------------------
%%% Get unique MFA list out of either mocker_state or a mock list
%%% ----------------------------------------------------------------------------
unique_mfa_list(#mocker_state{used_list = UsedList,
                              mock_list = MockList}) ->
    unique_mfa_list(UsedList ++ MockList);
unique_mfa_list(MockList) ->
    lists:usort([Mfa || #mock{mfa = Mfa} <- MockList]).

