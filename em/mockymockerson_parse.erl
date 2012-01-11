
-module(mockymockerson_parse).

-export([ parse_transform/2
        ]).

-include("mockymockerson_em_private.hrl").

%% -----------------------------------------------------------------------------
%% parse transform
%% -----------------------------------------------------------------------------
parse_transform(Forms, _Options) ->
    translate(Forms, [],[]).

translate([F = {attribute, LINE, module, _Mod} | Rest], Acc, Records) ->
    translate(Rest,
              [{attribute, LINE, export, [{?rec_fields, 1}]}, F | Acc],
              Records);

translate([F = {attribute, LINE, record, Rec} | Rest], Acc, Records) ->
    [Name, Fields] = tuple_to_list(Rec),
    FieldNames = [ 
        element(3, element(3, Field)) || 
            Field <- Fields, 
            record_field == element(1,Field), 
            atom == element(1,element(3,Field))
    ],
    translate(Rest, [F | Acc], [{Name, LINE, FieldNames} | Records]);

translate([F = {eof, LINE} | Rest], Acc, Records) ->
    translate(Rest, [F, make_rec_fields_function(Records, LINE) | Acc ], []);

translate([F | Rest], Acc, Records) ->
    translate(Rest, [F | Acc], Records);

translate([], Acc, []) ->
    lists:reverse(Acc).

make_rec_fields_function(Records, LINE) ->
    MkAtomList = fun(Names) ->
                    lists:foldl(fun(N,Acc) ->
                                    {cons, LINE, {atom, LINE, N}, Acc}
                                end,
                                {nil,LINE},
                                lists:reverse(Names))
                 end,
    Clauses =
        [{clause, L, [{atom, LINE, Name}], [], [MkAtomList(FieldNames)]} ||
         {Name, L, FieldNames} <- Records ]
        ++
        [ {clause, LINE, [{var, LINE, '_'}], [], [{atom, LINE, undefined}]}],
    {function, LINE, ?rec_fields, 1, Clauses}.

