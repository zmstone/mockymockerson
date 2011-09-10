
%% read record from given source code file

-module(rr_file).

-export([run/2]).

-include_lib("kernel/include/file.hrl").

run(File, Options) ->
    case filename:extension(File) of
        ".beam" ->
            rr_beam:run(File, Options);
        _ErlOrSth ->
            rr_src:run(File, Options)
    end.

