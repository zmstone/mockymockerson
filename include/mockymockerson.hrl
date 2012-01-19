
-ifndef(MOCKYMOCKERSON_HRL_).
-define(MOCKYMOCKERSON_HRL_, true).

-include_lib("eunit/include/eunit.hrl").

-include("mockymockerson_em.hrl").

%%
%% Whatever = {ArgList, Result}                        |
%%            {Arity, Result}                          |
%%            [{ArgList, Result}, {Arity, Result} ...] |
%%            Function
-define(mock(Mod, Fun, Whatever),
        mockymockerson:mock(?MODULE, ?LINE, Mod, Fun, Whatever)).

-define(mock_n(N, Mod, Fun, Whatever),
        mockymockerson:mock_n(N, ?MODULE, ?LINE, Mod, Fun, Whatever)).

-ifdef(assertEqual).
-undef(assertEqual).
-define(assertEqual(Expected, Expr), ?equal(Expected, Expr)).
-endif.

-ifdef(assertMatch).
-undef(assertMatch).
-define(assertMatch(Pattern, Expr), ?match(Pattern, Expr)).
-endif.

-endif.

