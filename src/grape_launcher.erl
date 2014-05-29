%%%-------------------------------------------------------------------
%%% @author belk
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. апр 2014 22:47
%%%-------------------------------------------------------------------
-module(grape_launcher).
-author("belk").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0]).


start() ->
    ok = lager:start(),
    ok = application:start(grape),
    lager:info("Starting whole grape server"),
    ok.


-ifdef(TEST).

prepare() ->
    start(),

    ?assertNot(undefined == whereis(grape_sup)).

finish() ->
    ok = application:stop(grape).

grape_test_() ->
    {
        setup,
        fun() -> prepare() end,
        fun(_) -> finish() end,
        fun() ->
            simple(),
            subscriber()
        end
    }.

simple() ->
    {ok, Common} = grape_sup:start_channel(common),
    {ok, Common} = grape_sup:start_channel(common),

    Common = grape_channel_manager:find_or_create(common),
    lager:info("Common pid: ~p~n", [Common]),
    ?assert(grape_util:is_pid_alive(Common) =:= true),

    ok = grape_channel:subscribe(common),

    Msg = "Test message",
    grape_channel:publish(common, Msg),
    receive
        {message, Id, Msg} ->
            lager:info("Message id: ~p~n", [Id])
    after
        100 -> throw(timeout)
    end,

    grape_channel:unsubscribe(common),

    ok.

subscriber() ->
    Id = <<"09309021-sdfsdfsd09fds">>,
    Pid = grape_subscriber_manager:find_or_create(Id, [common], []),
    grape_subscriber:listen(Pid, self()),
    Msg = "Subscriber message",
    grape_channel:publish(common, Msg),
    receive
        {messages, [{MsgId, Msg}]} ->
            lager:info("Subscriber received msg: ~p, ~p~n", [MsgId, Msg])
    after
        100 -> throw(timeout)
    end,



    ok.
-endif.