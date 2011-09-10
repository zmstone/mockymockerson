
-ifdef(MYMATCH_HRL).
-define(MYMATCH_HRL, true).

-define(match(A, B), mymatch:run(?MODULE, ?LINE, A, B)).

-endif.

