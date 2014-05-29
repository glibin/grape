-module(grape_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    install(),
    grape_sup:start_link().

stop(_State) ->
    grape_sup:stop(),
    application:stop(mnesia).

install() ->
    install([node()]). % |nodes()

install(Nodes) ->
    case mnesia:create_schema(Nodes) of
        ok -> ok;
        {error, Reason} ->
            lager:info("Mnesia create schema error: ~p~n", [Reason])
    end,
    application:start(mnesia),
    grape_channel_manager:create_table(Nodes),
    grape_subscriber_manager:create_table(Nodes),
    ok.