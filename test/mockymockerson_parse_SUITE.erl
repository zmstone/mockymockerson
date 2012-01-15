
-module(mockymockerson_parse_SUITE).

-export(
    [ all/0
    , t_all/1
    ]).

-include("mockymockerson.hrl").
-include("../em/mockymockerson_em_private.hrl").

-record(a, {b, c, d, e, f}).
-record(b, {c, d, e, f}).
-record(x, {y :: integer(),
            z :: string()}).

all() ->
    [ t_all
    ].

%%% ----------------------------------------------------------------------------
%%% all the tests are here
%%% ----------------------------------------------------------------------------
t_all(Conf) when is_list(Conf) ->
    ?assertEqual(record_info(fields, a), ?rec_fields(a)),
    ?assertEqual(record_info(fields, b), ?rec_fields(b)),
    ?assertEqual(record_info(fields, x), ?rec_fields(x)),
    ?assertEqual(undefined, ?rec_fields(rec_not_defined)),
    ok.

