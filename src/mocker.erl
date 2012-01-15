
%%%
%%% mocker the worker, dedicated to one mocked module
%%%

-module(mocker).

-behavior(gen_server).

-export([
     start/0
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

%%% ----------------------------------------------------------------------------
%%% start a mocker
%%% ----------------------------------------------------------------------------
start() ->
    gen_server:start_link(?MODULE, _Args = [], _Options = []).

%%% ----------------------------------------------------------------------------
%%% gen_server callback init function
%%% ----------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #mocker_state{}}.

%%% ----------------------------------------------------------------------------
%%% handle cast
%%% ----------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%% ----------------------------------------------------------------------------
%%% handle info
%%% ----------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%% ----------------------------------------------------------------------------
%%% terminate
%%% ----------------------------------------------------------------------------
terminate(_Reason, State) ->
    catch mockerson:clear(State),
    ok.

%%% ----------------------------------------------------------------------------
%%% code change
%%% ----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------------------------
%%% handle calls
%%% ----------------------------------------------------------------------------
handle_call(clear, _From, State) ->
    {Result, NewState} = mockerson:clear(State),
    {reply, Result, NewState};
handle_call(stop, _From, State) ->
    {Result, NewState} = mockerson:clear(State),
    {stop, normal, Result, NewState};
handle_call(#mock{} = Mock,  _From, State) ->
    case mockerson:mock(Mock, State) of
    {ok, NewState} ->
        {reply, ok, NewState};
    {fault, Reason} ->
        {reply, {fault, Reason}, State}
    end;
handle_call(#exec{} = Exec, _From, State) ->
    try mockerson:exec(Exec, State) of
    {Result, NewState} ->
        {reply, Result, NewState}
    catch
    throw:Exception ->
        {reply, Exception, State}
    end.

