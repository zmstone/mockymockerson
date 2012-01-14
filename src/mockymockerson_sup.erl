
%%%
%%% the supervisor
%%%

-module(mockymockerson_sup).

-export([
     start/1
    ,init/1
    ,dispatch/1
    ,clear/0
]).

-include("mockymockerson_private.hrl").

-define(mockers, mockymockerson_workers).

%%% ----------------------------------------------------------------------------
%%% external call
%%% ----------------------------------------------------------------------------
start(_Args) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ?mockers= ets:new(?mockers, [named_table, public]),
    {ok, Pid}.

%%% ----------------------------------------------------------------------------
%%% supervisor callback
%%% ----------------------------------------------------------------------------
init([]) ->
    MockerSpec = { ignored
                 , {mocker, start, []}
                 , temporary
                 , brutal_kill
                 , worker
                 , [mocker, mockerson] },

    {ok, {{simple_one_for_one, 0, 1}, [MockerSpec]}}.

%%% ----------------------------------------------------------------------------
%%% dispatch mocks or calls to the corresponding worker
%%% ----------------------------------------------------------------------------
dispatch(#mock{mfa = {M, _F, _A}} = Mock) ->
    Worker =
        case ets:lookup(?mockers, M) of
        [{M, TmpWorker}] ->
            TmpWorker;
        _ ->
            {ok, Pid} = supervisor:start_child(?MODULE, []),
            ets:insert(?mockers, {M, Pid}),
            Pid
        end,
    dispatch(Worker, Mock);
dispatch(#call{mfa = {M, _F, _A}} = Call) ->
    [{M, Worker}] = ets:lookup(?mockers, M),
    dispatch(Worker, Call).

%%% ----------------------------------------------------------------------------
%%% worker is identified, now dispatch it
%%% ----------------------------------------------------------------------------
dispatch(Worker, #mock{} = Mock) ->
    case gen_server:call(Worker, Mock) of
        ok ->
            ok;
        {fault, Reason} ->
            throw(Reason)
    end;
dispatch(Worker, #call{} = MockCall) ->
    case gen_server:call(Worker, MockCall) of
        {?exception, Exception} ->
            throw(Exception);
        ReturnValue ->
            ReturnValue
    end.

%%% ----------------------------------------------------------------------------
%%% stop all the mockers
%%% ----------------------------------------------------------------------------
clear() ->
    Mockers = [Mocker || {_Module, Mocker} <- ets:tab2list(?mockers)],
    ets:delete_all_objects(?mockers),
    ExtraMocks = [gen_server:call(Mocker, clear) || Mocker <- Mockers],
    [catch gen_server:call(Mocker, stop) || Mocker <- Mockers],
    lists:append(ExtraMocks).

