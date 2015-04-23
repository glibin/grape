%%%-------------------------------------------------------------------
%%% @author belk
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. апр 2014 23:12
%%%-------------------------------------------------------------------
-module(grape_channel).
-author("belk").

-behaviour(gen_server).

%% API
-export([start_link/1, find/1, publish/2, subscribe/1, unsubscribe/1, cast/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    subscribers = []    :: list(),
    name                :: term()
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
-spec start_link(Name :: term()) -> Result :: {ok, pid()} | {error, term()}.
start_link(Name) ->
    gen_server:start_link(?MODULE, Name, []).


find(Pid) when is_pid(Pid) ->
    Pid;

find(Name) ->
    grape_channel_manager:find_or_create(Name).

publish(Name, Msg) ->
    Id = grape_util:full_ts(),
    Pid = find(Name),
    gen_server:cast(Pid, {publish, Id, Msg}),
    [rpc:cast(Node, grape_channel, cast, [Name, Id, Msg]) || Node <- nodes()],
    Id.

cast(Name, Id, Msg) ->
    case grape_channel_manager:find(Name) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {publish, Id, Msg})
    end.

subscribe(Name) ->
    lager:info("Name: ~p, pid: ~p~n", [Name, find(Name)]),
    gen_server:call(find(Name), {subscribe, self()}).

unsubscribe(Name) ->
    gen_server:call(find(Name), {unsubscribe, self()}).

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
init(Name) ->
    {ok, #state{name = Name}}.

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

handle_call({subscribe, Pid}, _From, State=#state{name=Name, subscribers=Subscribers}) ->
    Ref = erlang:monitor(process, Pid),
    NewSubscribers = [{Pid, Ref} | Subscribers],
    lager:info("Number of subscribers for the channel ~p is ~p~n", [Name, length(NewSubscribers)]),
    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call({unsubscribe, Pid}, _From, State=#state{name=Name, subscribers=Subscribers}) ->
    NewSubscribers = case find_pid(Pid, Subscribers) of
        undefined ->
            Subscribers;
        Ref ->
            erlang:demonitor(Ref),
            Subscribers -- [{Pid, Ref}]
    end,
    lager:info("Number of subscribers for the channel ~p is ~p~n", [Name, length(NewSubscribers)]),
    {reply, ok, State#state{subscribers = NewSubscribers}};

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

handle_cast({publish, Id, Msg}, State=#state{name=Name, subscribers=Subscribers}) ->
    % lager:info("New message: ~p~n", [Msg]),
    [Sub ! {message, Id, Msg} || {Sub, _Ref} <- Subscribers],
    {noreply, State};

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

handle_info({'DOWN', Ref, _, Pid, _}, State=#state{subscribers=Subscribers}) ->
    NewSubscribers = Subscribers -- [{Pid, Ref}],
    case NewSubscribers of
        [] -> {noreply, State#state{subscribers = []}, hibernate};
        _ -> {noreply, State#state{subscribers = NewSubscribers}}
    end;

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

find_pid(_Pid, []) ->
    undefined;

find_pid(Pid, [{Pid, Ref}|_]) ->
    Ref;

find_pid(Pid, [Item|Items]) ->
    find_pid(Pid, Items).
