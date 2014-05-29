%%%-------------------------------------------------------------------
%%% @author belk
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. апр 2014 10:43
%%%-------------------------------------------------------------------
-module(grape_subscriber).
-author("belk").

-behaviour(gen_server).

-define(TIMEOUT, 30000).

%% API
-export([start_link/3, listen/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    id,
    channels=[],
    options=[],
    messages=[],
    listener=undefined,
    timeout=undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Name :: binary(), Channels :: list(), Options :: list()) -> Result :: {ok, pid()} | {error, term()}.
start_link(Name, Channels, Options) ->
    gen_server:start_link(?MODULE, [Name, Channels, Options], []).

listen(Name, Waiter) ->
    case grape_subscriber_manager:find(Name) of
        undefined -> {error, no_subscriber};
        Pid -> gen_server:cast(Pid, {listen, Waiter})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Name, Channels, Options]) ->
    [grape_channel:subscribe(Channel) || Channel <- Channels],
    Timer = erlang:send_after(?TIMEOUT, self(), timeout),
    {ok, #state{id=Name, channels = Channels, options = Options, timeout = Timer}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({listen, Waiter}, State=#state{listener = OldWaiter, messages = Messages, timeout = Timer})->
    case OldWaiter of
        undefined -> ok;
        W -> W ! {messages, []}
    end,
    erlang:monitor(process, Waiter),
    grape_util:cancel_timer(Timer),
    case Messages of
        [] ->
            {noreply, State#state{listener = Waiter, timeout = undefined}};
        M ->
            Waiter ! {messages, M},
            {noreply, State#state{listener = Waiter, messages = [], timeout = undefined}}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({message, Id, Msg}, State=#state{listener = Waiter, messages = OldMessages}) ->
    lager:info("Gotcha new info msg ~p : ~p~n", [Id, Msg]),
    NewMessages = case Waiter of
        undefined -> OldMessages ++ [{Id, Msg}];
        W ->
            lager:info("This: ~p~n", [OldMessages ++ [{Id, Msg}]]),
            W ! {messages, OldMessages ++ [{Id, Msg}]},
            []
    end,
    {noreply, State#state{messages = NewMessages}};

handle_info({'DOWN', _, _, Pid, _}, State=#state{listener = Pid}) ->
    lager:info("DOWN listener ~p~n", [Pid]),
    Timer = erlang:send_after(?TIMEOUT, self(), timeout),
    {noreply, State#state{listener = undefined, timeout = Timer}};

handle_info(timeout, State=#state{timeout = Timer}) ->
    erlang:cancel_timer(Timer),
    {stop, normal, State#state{timeout = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
