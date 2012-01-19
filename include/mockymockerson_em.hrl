
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
                    %% If there are Variables in Pattern, they'll be shadowed 
                    try ets:fun2ms(fun({Pattern}) -> ok end) of
                    [{{__MatchSpec}, [], [ok]}] ->
                        __Fixed = mockymockerson_ignore:fix(__MatchSpec, __Value),
                        ?equal(__Fixed, __Value)
                    catch
                    _:_ ->
                        %% in case some stupid use of pattern
                        throw({mismatch, Pattern = Value})
                    end
                end
            end
         )()
       ).

-endif.

