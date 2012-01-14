
%%%
%%% the supervisor
%%%

-module(mockymockerson_sup).

-export([
     start/1
    ,init/1
]).

-include("mockymockerson_private.hrl").

-define(workers, mockymockerson_workers).

%%% ----------------------------------------------------------------------------
%%% external call
%%% ----------------------------------------------------------------------------
start(_Args) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    supervisor:start_child(?MODULE, []),
    {ok, Pid}.

%%% ----------------------------------------------------------------------------
%%% supervisor callback
%%% ----------------------------------------------------------------------------
init([]) ->
    MockySpec = { ignored
                , {mocky, start, []}
                , temporary
                , brutal_kill
                , worker
                , [mocky, mockerson] },

    {ok, {{simple_one_for_one, 0, 1}, [MockySpec]}}.

