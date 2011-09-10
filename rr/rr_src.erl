
%% read record from given source code file

-module(rr_src).

-export([run/2]).

-include_lib("kernel/include/file.hrl").

%% -----------------------------------------------------------------------------
%% read records
%% Options = [{i, IncludePath}, {d, Macro}, {d, Macro, Value} | ...]
%% -----------------------------------------------------------------------------
run(File, Options) when is_list(File) ->
    case parse(File, Options) of
        RecordList when is_list(RecordList) ->
            rr_lib:records(RecordList);
        Error ->
            Error
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
parse(File, Opts) ->
    Dir = filename:dirname(File),
    IncludePath = [".", Dir | inc_paths(Opts)],
    case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
        {ok, Forms} ->
            rr_lib:attrs(Forms);
        Error ->
            Error
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
pre_defs([]) -> [];
pre_defs([{d, M, V} | Opts]) ->
    [{M, V} | pre_defs(Opts)];
pre_defs([{d, M} | Opts]) ->
    [M | pre_defs(Opts)];
pre_defs([_ | Opts]) ->
    pre_defs(Opts).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
inc_paths(Opts) ->
    [P || {i,P} <- Opts, is_list(P)].

