
-module(rr_lib).

-export([attrs/1,
         records/1
       ]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
attrs(Forms) ->
    [A || A = {attribute, _, record, _D} <- Forms].

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
records(RecordList) ->
    [{Tag, fields(RecordFields)} ||
        {attribute, _line, record, {Tag, RecordFields}} <- RecordList].

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
fields(RfList) ->
    [element(3, element(3, Rf)) || Rf <- RfList].
