
%%%
%%% the supervisor
%%%

-module(mockymockerson_sup).

-export([
     start/1
    ,init/1
    ,dispatch/1
]).

-include("mockymockerson_private.hrl").

-define(workers, mockymockerson_workers).

%%% ----------------------------------------------------------------------------
%%% external call
%%% ----------------------------------------------------------------------------
start(_Args) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    supervisor:start_child(?MODULE, []),
    ?workers = ets:new(?workers, [named_table, public]),
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

dispatch(#mock{} = Mock) ->
    case gen_server:call(?SERVER, Mock) of
        ok ->
            ok;
        {fault, Reason} ->
            throw(Reason)
    end;
dispatch(#mock_call{} = MockCall) ->
    case gen_server:call(?SERVER, MockCall) of
        {?exception, Exception} ->
            throw(Exception);
        ReturnValue ->
            ReturnValue
    end.

