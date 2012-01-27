
-ifndef(MOCKYMOCKERSON_EM_HRL_).
-define(MOCKYMOCKERSON_EM_HRL_, true).

-include_lib("stdlib/include/ms_transform.hrl").

-compile({parse_transform, mockymockerson_parse}).

-define( equal(A, B),
         mockymockerson_match:run(?MODULE, ?LINE, A, B, [{print, all}])
       ).

-define( equal_opt(A, B, Opt),
         mockymockerson_match:run(?MODULE, ?LINE, A, B, Opt)
       ).

-define( match(Pattern, Value),
         ( fun() ->
                case (Value) of
                Pattern ->
                    ok;
                __Value ->
                    %% Pattern is parsed into Variable by mockymockerson_parse
                    __Parsed = {'$MOCKYMOCKERSON_PARSE_ME', ??Pattern},
                    __Fixed = mockymockerson_ignore:fix(__Parsed, __Value),
                    %% let ?equal report the match details
                    ?equal(__Fixed, __Value)
                end
            end
         )()
       ).

-endif.

