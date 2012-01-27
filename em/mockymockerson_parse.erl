
-module(mockymockerson_parse).

-export([ parse_transform/2
        ]).

-include("mockymockerson_em_private.hrl").

%% -----------------------------------------------------------------------------
%% parse transform
%% -----------------------------------------------------------------------------
parse_transform(Forms, _Options) ->
    scan(Forms, [], []).

%% -----------------------------------------------------------------------------
%% traverse through the abstraction code, find all the defined records
%% -----------------------------------------------------------------------------
scan([], Acc, []) ->
    lists:reverse(Acc);

scan([F = {attribute, LINE, module, _Mod} | Rest], Acc, Records) ->
    scan(Rest,
         [{attribute, LINE, export, [{?rec_fields, 1}]}, F | Acc],
         Records);

scan([F = {attribute, _LINE, record, {Rec, _}} | Rest], Acc, Records) ->
    scan(Rest, [F | Acc], [Rec | Records]);

scan([F = {function, _, _, _, _} | Rest], Acc, Records) ->
    scan(Rest, [scan_pattern(F) | Acc], Records);

scan([F = {eof, LINE} | Rest], Acc, Records) ->
    scan(Rest, [F, make_rec_fields_function(Records, LINE) | Acc], []);

scan([F | Rest], Acc, Records) ->
    scan(Rest, [F | Acc], Records).

%% -----------------------------------------------------------------------------
%% make the secretly exported function ?rec_fields/1
%% -----------------------------------------------------------------------------
make_rec_fields_function(Records, L) ->
    Clauses =
    [
        {clause, L,
            [{atom, L, Name}], [],
            [
                {call, L,
                    {atom, L, record_info},
                    [{atom, L, fields}, {atom, L, Name}]
                }
            ]
         } || Name <- Records
    ]
    ++
    [ {clause, L, [{var, L, '_'}], [], [{atom, L, undefined}]} ],
    {function, L, ?rec_fields, 1, Clauses}.

%% -----------------------------------------------------------------------------
%% find the pattern blow, and translate it to variable
%% {tuple,_,[{atom,_,'$MOCKYMOCKERSON_PARSE_ME'},{string,_,PatternStr}]}
%% -----------------------------------------------------------------------------
scan_pattern({tuple, _, [{atom, _, '$MOCKYMOCKERSON_PARSE_ME'},
                         {string, Line, PatternStr}]}) ->
    translate(Line, PatternStr);
scan_pattern(F) when is_list(F) ->
    [scan_pattern(I) || I <- F];
scan_pattern(F) when is_tuple(F) ->
    list_to_tuple([scan_pattern(I) || I <- tuple_to_list(F)]);
scan_pattern(F) ->
    F.

%% -----------------------------------------------------------------------------
%% translate pattern string to variable
%% -----------------------------------------------------------------------------
translate(Line, PatternStr) ->
  {ok, Tokens, Line} = erl_scan:string(PatternStr ++ ".", Line),
  {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
  translate_var_underscore(Expr).

%% -----------------------------------------------------------------------------
%% translate {var, Line, '_'} to {atom, Line, '_'}
%% -----------------------------------------------------------------------------
translate_var_underscore({var, Line, '_'}) ->
  {atom, Line, '_'};
translate_var_underscore(E) when is_list(E) ->
  [translate_var_underscore(I) || I <- E];
translate_var_underscore(E) when is_tuple(E) ->
  list_to_tuple([translate_var_underscore(I) || I <- tuple_to_list(E)]);
translate_var_underscore(E) ->
  E.

