
%% read record

-module(rr).

-export([run/1,
         run/2]).

%% -----------------------------------------------------------------------------
%% read record by specifying the module name or the source code file path name
%% -----------------------------------------------------------------------------
run(ModuleOrFile) ->
    run_help(ModuleOrFile, []).

%% -----------------------------------------------------------------------------
%% read record by specifying the compilation options
%% Options = [{i, IncludePath}, {d, Macro}, {d, Macro, Value} | ...]
%% -----------------------------------------------------------------------------
run(ModuleOrFile, Options) ->
    run_help(ModuleOrFile, lists:delete(report_warnings, Options)).

run_help(Module, Options) when is_atom(Module) ->
    rr_module:run(Module, Options);
run_help(SrcFile, Options) when is_list(SrcFile) ->
    rr_file:run(SrcFile, Options).

