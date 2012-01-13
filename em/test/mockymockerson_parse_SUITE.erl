
-module(mockymockerson_parse_SUITE).

-include("mockymockerson.hrl").
-include("../mockymockerson_em_private.hrl").

-record(a, {b, c, d, e, f}).
-record(b, {c, d, e, f}).
-record(x, {y :: integer(),
            z :: string()}).

all_test() ->
    ?assertEqual(record_info(fields, a), '$MOCKYMOCKERSON_RECORD_FIELDS'(a)),
    ?assertEqual(record_info(fields, b), '$MOCKYMOCKERSON_RECORD_FIELDS'(b)),
    ?assertEqual(record_info(fields, x), '$MOCKYMOCKERSON_RECORD_FIELDS'(x)),
    ?assertEqual(undefined, '$MOCKYMOCKERSON_RECORD_FIELDS'(rec_not_defined)),
    ok.

