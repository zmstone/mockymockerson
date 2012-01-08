
-ifndef(MOCKYMOCKERSON_EM_HRL_).
-define(MOCKYMOCKERSON_EM_HRL_, true).

-compile({parse_transform, mockymockerson_parse}).

-define(match(A, B),
        mockymockerson_match:run(?MODULE, ?LINE, A, B, [{print, all}])).
-define(match_opt(A, B, Opt),
        mockymockerson_match:run(?MODULE, ?LINE, A, B, Opt)).

-if
-endif.

