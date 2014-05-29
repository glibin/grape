-module(grape_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0, start_channel/1, start_subscriber/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    erlang:exit(erlang:whereis(?MODULE), shutdown).

start_channel(Name) ->
    ChannelSpec = {Name, {supervisor, start_link, [?MODULE, {channel, Name}]}, permanent, infinity, supervisor, []},
    {ok, Supervisor} = case supervisor:start_child(grape_channels_sup, ChannelSpec) of
                           {ok, SupPid} -> {ok, SupPid};
                           {error, {already_started, SupPid}} -> {ok, SupPid}
                       end,
    {channel, Pid, _, _} = lists:keyfind(channel, 1, supervisor:which_children(Supervisor)),
    {ok, Pid}.

start_subscriber(Name, Channels, Options) ->
    SubscriberSpec = {Name, {grape_subscriber, start_link, [Name, Channels, Options]}, transient, 5000, worker, [grape_subscriber]},
    case supervisor:start_child(grape_subscriber_sup, SubscriberSpec) of
        {ok, P} -> {ok, P};
        {error, already_present} ->
            supervisor:delete_child(grape_subscriber_sup, Name),
            start_subscriber(Name, Channels, Options);
        {error, {already_started, P}} ->
            {ok, P}
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ChannelsSup = {grape_channels_sup,
        {supervisor, start_link, [{local, grape_channels_sup}, ?MODULE, [channels]]},
        permanent, infinity, supervisor, dynamic
    },
    ChannelsMgr = {grape_channel_manager,
        {grape_channel_manager, start_link, []},
        permanent, 5000, worker, [grape_channel_manager]
    },
    SubscribersSup = {grape_subscriber_sup,
        {supervisor, start_link, [{local, grape_subscriber_sup}, ?MODULE, [subscribers]]},
        permanent, infinity, supervisor, dynamic
    },
    SubscribersMgr = {grape_subscriber_manager,
        {grape_subscriber_manager, start_link, []},
        permanent, 5000, worker, [grape_subscriber_manager]
    },
    {ok, { {one_for_one, 5, 10}, [ChannelsMgr, ChannelsSup, SubscribersMgr, SubscribersSup]} };

init({channel, Name}) ->
    {ok, {{one_for_all, 5, 10}, [
        % {replicator, {dps_channel_replicator, start_link, [Name]}, permanent, 5000, worker, [dps_channel_replicator]},
        {channel, {grape_channel, start_link, [Name]}, permanent, 5000, worker, [grape_channel]}
    ]}};

init([channels]) ->
    {ok, { {one_for_one, 5, 10}, []} };

init([subscribers]) ->
    {ok, { {one_for_one, 5, 10}, []} }.