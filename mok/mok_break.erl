
-module(mok_break).

-export([pause/1]).

-include("mockymockerson_private.hrl").

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
pause(MockCall) ->
    case get(pause_mock) of
        true ->
            send_trace(MockCall),
            wait_response();
        _ ->
            do_nothing
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
send_trace(_MockCall) ->
    Trace = [], %% trace the call here
    case get(remote_debuger) of
        true ->
            io:format("remote debuger not implemented\n");
        _ ->
            self() ! Trace
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
wait_response() ->
    receive
        resume ->
            ok; %% received a resume command
        TraceInfo ->
            io:format("trace:\n~p\npress r to resume\n", [TraceInfo]),
            wait_input()
    end.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
wait_input() ->
    case io:get_chars("> ", 1) of
        "r" ->
            self() ! resume;
        _ ->
            wait_input()
    end.

