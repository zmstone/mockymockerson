
%% read record from given module

-module(rr_module).

-export([run/2]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
run(Module, Options) when is_atom(Module) ->
    case src_file(Module) of
        File when is_list(File) ->
            rr_file:run(File, Options);
        Error ->
            Error
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
src_file(Module) when is_atom(Module) ->
    case code:which(Module) of
        BeamFile when is_list(BeamFile) ->
            src_file(BeamFile);
        preloaded ->
            {_M, _Bin, BeamFile} = code:get_object_code(Module),
            src_file(BeamFile);
        _ -> 
            {error, no_file}
    end;
src_file(BeamFile) when is_list(BeamFile) ->
    SrcFile = filename:rootname(BeamFile) ++ ".erl",
    case is_file(SrcFile) of
        true ->
            SrcFile;
        false ->
            {error, no_file}
    end.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
is_file(Name) ->
    filelib:is_file(Name) andalso not filelib:is_dir(Name).

