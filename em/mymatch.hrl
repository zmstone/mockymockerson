
-ifndef(MYMATCH_HRL).
-define(MYMATCH_HRL, true).

-compile({parse_transform, mymatch}).

-define(match(A, B),          mymatch:run(?MODULE, ?LINE, A, B, [{print, all}])).
-define(match_opt(A, B, Opt), mymatch:run(?MODULE, ?LINE, A, B, Opt)).

-endif.

