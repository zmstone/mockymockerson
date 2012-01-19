
-module(mockymockerson_parse_SUITE).

-export(
    [ all/0
    , t_record/1
    , t_equal/1
    , t_match/1
    , t_match_more/1
    ]).

-include("mockymockerson.hrl").
-include("../em/mockymockerson_em_private.hrl").

-record(a, {b, c, d, e, f}).
-record(b, {c, d, e, f}).
-record(x, {y :: integer(),
            z :: string()}).

all() ->
    [ t_record
    , t_equal
    , t_match
    , t_match_more
    ].

%%% ----------------------------------------------------------------------------
%%% test reading record fields
%%% ----------------------------------------------------------------------------
t_record(Conf) when is_list(Conf) ->
    ?assertEqual(record_info(fields, a), ?rec_fields(a)),
    ?assertEqual(record_info(fields, b), ?rec_fields(b)),
    ?assertEqual(record_info(fields, x), ?rec_fields(x)),
    ?assertEqual(undefined, ?rec_fields(rec_not_defined)),
    ok.

%%% ----------------------------------------------------------------------------
%%% Test equal macro
%%% ----------------------------------------------------------------------------
t_equal(Conf) when is_list(Conf) ->
    {mismatch, _} = (catch ?assertEqual('_', {a})),
    ?assertEqual([or_b], [or_b]),
    ?assertEqual("or_whatsoever", "or_whatsoever"),
    ok.

%%% ----------------------------------------------------------------------------
%%% Test match macro
%%% ----------------------------------------------------------------------------
t_match(Conf) when is_list(Conf) ->
    ?assertMatch(_a, {a}),
    ?assertMatch(_orb, [orb]),
    ?assertMatch(_orwhatsoever, "orwahtsoever"),
    ?assertMatch(#a{}, #a{b = hasvalue}),
    {mismatch, _} = (catch ?assertMatch(#a{b = hsavalue}, #a{b = hasvalue})),
    ?assertMatch(#a{b = _DummAssDoItLikeThis}, #a{b = hasvalue}),
    {mismatch, _} = (catch ?assertMatch([#a{}, "something"],
                                        [#a{d = value}, "somethingelse"])),
    ok.

%%% ----------------------------------------------------------------------------
%%% Test match macro some more
%%% ----------------------------------------------------------------------------
t_match_more(Conf) when is_list(Conf) ->
    X = "something",
    {mismatch, _} = (catch ?assertMatch(X, "somethingelse")),
    ok.

