
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

-define(ct(CASE), {CASE, fun(ARG) -> CASE(ARG) end}).

-endif.

