
%% read record from given beam file

-module(rr_beam).

-export([run/2]).

run(File, _Options) ->
    case parse(File) of
        RecordList when is_list(RecordList) ->
            rr_lib:records(RecordList);
        Error ->
            Error
    end.

parse(File) ->
    case beam_lib:chunks(File, [abstract_code, "CInf"]) of
        {ok, {_Mod, [{abstract_code, {_V, Forms}}, {"CInf", _CB}]}} ->
            rr_lib:attrs(Forms);
        _ ->
            no_abstract_code_in_beam
    end.

