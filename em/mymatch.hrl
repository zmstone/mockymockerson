
-ifdef(MYMATCH_HRL).
-define(MYMATCH_HRL, true).

-define(mymatch(A, B), mymatch:run(?MODULE, ?LINE, A, B)).

-endif.

