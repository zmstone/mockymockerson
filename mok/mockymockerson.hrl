
-ifndef(MOCKYMOCKERSON_HRL_).
-define(MOCKYMOCKERSON_HRL_, true).

-define(exception, mockymockerson_exception).

%%
%% Whatever = {ArgList, Result}                        |
%%            {Arity, Result}                          |
%%            [{ArgList, Result}, {Arity, Result} ...] |
%%            Function
-define(mock(Mod, Fun, Whatever),
        mockymockerson:mock(?MODULE, ?LINE, Mod, Fun, Whatever)).

-endif.

