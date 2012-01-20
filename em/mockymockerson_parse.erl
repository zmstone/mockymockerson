
-module(mockymockerson_parse).

-export([ parse_transform/2
        ]).

-include("mockymockerson_em_private.hrl").

%% -----------------------------------------------------------------------------
%% parse transform
%% -----------------------------------------------------------------------------
parse_transform(Forms, _Options) ->
    translate(Forms, [],[]).

%% -----------------------------------------------------------------------------
%% traverse through the abstraction code, find all the defined records
%% -----------------------------------------------------------------------------
translate([], Acc, []) ->
    lists:reverse(Acc);

translate([F = {attribute, LINE, module, _Mod} | Rest], Acc, Records) ->
    translate(Rest,
              [{attribute, LINE, export, [{?rec_fields, 1}]}, F | Acc],
              Records);

translate([F = {attribute, _L, record, {Rec, Fields}} | Rest], Acc, Records) ->
    translate(Rest, [F | Acc], [{Rec, length(Fields)} | Records]);

translate([F = {eof, LINE} | Rest], Acc, Records) ->
    translate(Rest, [F, make_rec_fields_function(Records, LINE) | Acc], []);

translate([F | Rest], Acc, Records) ->
    translate(Rest, [F | Acc], Records).

%% -----------------------------------------------------------------------------
%% make up the secretly exported function ?rec_fields/1
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
         } || {Name, _Size} <- Records
    ]
    ++
    [ {clause, L, [{var, L, '_'}], [], [{atom, L, undefined}]} ],
    {function, L, ?rec_fields, 1, Clauses}.

