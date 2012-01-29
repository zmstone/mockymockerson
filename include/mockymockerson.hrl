
-ifndef(MOCKYMOCKERSON_HRL_).
-define(MOCKYMOCKERSON_HRL_, true).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlymatch/include/erlymatch.hrl").

%%
%% Whatever = {ArgList, Result}                        |
%%            {Arity, Result}                          |
%%            [{ArgList, Result}, {Arity, Result} ...] |
%%            Function
-define(mock(Mod, Fun, Whatever),
        mockymockerson:mock(?MODULE, ?LINE, Mod, Fun, Whatever)).

-define(mock_n(N, Mod, Fun, Whatever),
        mockymockerson:mock_n(N, ?MODULE, ?LINE, Mod, Fun, Whatever)).

-endif.

