
%%%
%%% mocky the server
%%%

-module(mocky).

-behavior(gen_server).

-export([
     start/0
    ,purge/0
    ,stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("mockymockerson_private.hrl").

-record(state, {}).

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
purge() ->
    gen_server:call(?SERVER, purge).

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------------------------
%%% ----------------------------------------------------------------------------
handle_call(purge, _From, State) ->
    try {reply, mockerson:purge(), State}
    catch
    throw:Exception ->
        {reply, Exception, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(#mock{} = Mock,  _From, State) ->
    case mockerson:mock(_Mockerson = void, Mock) of
        {ok, _NewMockerson} ->
            {reply, ok, State};
        {fault, Reason} ->
            {reply, {fault, Reason}, State}
    end;
handle_call(#mock_call{} = MockCall, _From, State) ->
    try {reply, mockerson:call(MockCall), State} 
    catch
    throw:Exception ->
        {reply, Exception, State}
    end.

