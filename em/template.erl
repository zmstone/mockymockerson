
-module(template).

-export([rec_fields/1]).

-record(a, {b,c}).
-record(b, {c}).

rec_fields(a) -> record_info(fields, a); rec_fields(b) -> record_info(fields, b).

%% the output of epp:parse_file("template.erl", [], [])
%% {function,9,rec_fields,1,
%%     [{clause,9,
%%         [{atom,9,a}],
%%         [],
%%         [{call,9,
%%             {atom,9,record_info},
%%             [{atom,9,fields},{atom,9,a}]}]},
%%      {clause,9,
%%         [{atom,9,b}],
%%         [],
%%         [{call,9,
%%             {atom,9,record_info},
%%             [{atom,9,fields},{atom,9,...}]}]}]},

