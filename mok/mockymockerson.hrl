
-ifndef(MOCKYMOCKERSON_HRL_).
-define(MOCKYMOCKERSON_HRL_, true).

%%
%% Whatever = {ArgList, Result}                        |
%%            {Arity, Result}                          |
%%            [{ArgList, Result}, {Arity, Result} ...] |
%%            Function
-define(mock(Mod, Fun, Whatever),
        mockymockerson:mock(?MODULE, ?LINE, Mod, Fun, Whatever)).

-define(mock_n(N, Mod, Fun, Whatever),
        mockymockerson:mock_n(N, ?MODULE, ?LINE, Mod, Fun, Whatever)).

-define(match(A, B), mymatch:run(?MODULE, ?LINE, A, B)).

-define(ct(CASE), fun(name) -> CASE;
                     (Other) -> CASE(Other) end).
-export([
    run/0,
    run/1,
    all/0
        ]).

run() ->
    run(all()).

run([]) ->
    ok;
run(Case) when is_atom(Case) ->
    run([Case]);
run([CaseFun | Rest]) ->
    case catch CaseFun(suite) of
        SubCaseList when is_list(SubCaseList) andalso SubCaseList /= [] ->
            run(SubCaseList);
        _ ->
            exec(CaseFun)
    end,
    run(Rest).

exec(CaseFun) ->
    Case = CaseFun(name),
    mockymockerson:start(),
    io:format("testing: ~p ...", [Case]),
    case catch CaseFun(exec) of
        ok ->
            io:format(" ok\n"),
            ok;
        Reason ->
            io:format(" failed\n"),
            io:format("~p~n", [Reason]),
            nok
    end,
    mockymockerson:stop().

-endif.

