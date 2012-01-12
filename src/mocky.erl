
-module(mocky).

-behavior(gen_server).

-export([
    start/0,
    stop/0
        ]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
        ]).

-include("mockymockerson_private.hrl").

-record(mocky, {state     = mocking    %% mocking | checking | terminating
                }).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, _Args = [], _Options = []).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
stop() ->
    gen_server:call(?SERVER, stop).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #mocky{}}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
handle_cast(_Msg, Mocky) ->
    {noreply, Mocky}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
handle_info(_Info, Mocky) ->
    {noreply, Mocky}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
code_change(_OldVsn, Mocky, _Extra) ->
    {ok, Mocky}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
handle_call(stop, _From, Mocky) ->
    {stop, normal, ok, Mocky};
handle_call(#mock{} = Mock,  _From, Mocky) ->
    case mockerson:mock(_Mockerson = void, Mock) of
        {ok, _NewMockerson} ->
            {reply, ok, Mocky};
        {fault, Reason} ->
            {reply, {fault, Reason}, Mocky}
    end;
handle_call(#mock_call{} = MockCall, _From, Mocky) ->
    try {reply, mockerson:call(MockCall), Mocky} 
    catch
    throw:Exception ->
        {reply, Exception, Mocky}
    end.

