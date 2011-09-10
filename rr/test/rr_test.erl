
-module(rr_test).

-export([run/0]).

-record(rec_normal, {a, b, c}).

-record(rec_dftvalue, {x=1, y=1+2, z=undef}).

-record(rec_rand, {e1 = random:uniform(100),
                   e2 = random:uniform(100)}).

-record(rec_empty, {}).

run() ->
    ExpectedRecords = [ {rec_normal,[a,b,c]},
                        {rec_dftvalue,[x,y,z]},
                        {rec_rand,[e1,e2]},
                        {rec_empty,[]}],
    ExpectedRecords = rr:run(?MODULE),
    ExpectedRecords = rr:run("rr_test.erl"),
    ExpectedRecords = rr:run("rr_test.beam"),
    ok.

