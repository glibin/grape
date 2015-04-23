%%%-------------------------------------------------------------------
%%% @author belk
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. апр 2014 16:15
%%%-------------------------------------------------------------------
-module(grape_channel_manager).
-author("belk").

-behaviour(gen_server).

%% API
-export([start_link/0, find_or_create/1, find/1, create_table/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(grape_channel_manager, {
    name,
    pid
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
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_table(Nodes) ->
    mnesia:create_table(grape_channel_manager,
        [{attributes, record_info(fields, grape_channel_manager)},
            {index, [#grape_channel_manager.pid]},
            {ram_copies, Nodes},
            {type, set}]).

find_or_create(Name) ->
    gen_server:call(?MODULE, {find_or_create, Name}).

find(Pid) when is_pid(Pid) ->
    case mnesia:dirty_index_read(grape_channel_manager, Pid, #grape_channel_manager.pid) of
        [#grape_channel_manager{name=Name}] ->
            Name;
        _ ->
            undefined
    end;

find(Name) ->
    case mnesia:dirty_read(grape_channel_manager, Name) of
        [#grape_channel_manager{pid=Pid}] ->
            Pid;
        _ ->
            undefined
    end.

delete(Pid) when is_pid(Pid) ->
    case mnesia:dirty_index_read(grape_channel_manager, Pid, #grape_channel_manager.pid) of
        [#grape_channel_manager{} = Record] ->
            mnesia:dirty_delete_object(Record);
        _ ->
            ok
    end;

delete(Name) ->
    case mnesia:dirty_read(grape_channel_manager, Name) of
        [#grape_channel_manager{} = Record] ->
            mnesia:dirty_delete_object(Record);
        _ ->
            ok
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
init(Args) ->
    {ok, Args}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
%% -spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
%%     State :: #grape_channel_manager{}) ->
%%     {reply, Reply :: term(), NewState :: #grape_channel_manager{}} |
%%     {reply, Reply :: term(), NewState :: #grape_channel_manager{}, timeout() | hibernate} |
%%     {noreply, NewState :: #grape_channel_manager{}} |
%%     {noreply, NewState :: #grape_channel_manager{}, timeout() | hibernate} |
%%     {stop, Reason :: term(), Reply :: term(), NewState :: #grape_channel_manager{}} |
%%     {stop, Reason :: term(), NewState :: #grape_channel_manager{}}).

handle_call({find_or_create, Name}, _From, State) ->
    F = fun() ->
        Pid2 = case mnesia:read(grape_channel_manager, Name) of
                   [{grape_channel_manager, Name, Pid}] ->
                       case grape_util:is_pid_alive(Pid) of
                           true ->
                               Pid;
                           false ->
                               {ok, Pid} = grape_sup:start_channel(Name),
                               erlang:monitor(process, Pid),
                               Pid
                       end;
                   [] ->
                       {ok, Pid} = grape_sup:start_channel(Name),
                       erlang:monitor(process, Pid),
                       Pid
               end,
        mnesia:write(#grape_channel_manager{
            name=Name,
            pid=Pid2
        }),
        Pid2
    end,
    P = mnesia:activity(transaction, F),
    {reply, P, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
%% -spec(handle_cast(Request :: term(), State :: #state{}) ->
%%     {noreply, NewState :: #state{}} |
%%     {noreply, NewState :: #state{}, timeout() | hibernate} |
%%     {stop, Reason :: term(), NewState :: #state{}}).
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
%% -spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
%%     {noreply, NewState :: #state{}} |
%%     {noreply, NewState :: #state{}, timeout() | hibernate} |
%%     {stop, Reason :: term(), NewState :: #state{}}).
handle_info({'DOWN', _, _, Pid, _}, State) ->
    lager:info("DOWN ~p~n", [Pid]),
    delete(Pid),
    {noreply, State};

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
%% -spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
%%     State :: #state{}) -> term()).
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
%% -spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
%%     Extra :: term()) ->
%%     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
