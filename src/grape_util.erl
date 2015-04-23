%% Copyright
-module(grape_util).
-author("belk").

%% API
-export([ts/0, full_ts/0, ms/0, cancel_timer/1, wait_until/1, wait_until/3, is_pid_alive/1]).

ts() ->
    {Megasecs, Secs, _} = now(),
    Secs + 1000000 * Megasecs.

ms() ->
    round(full_ts()/1000).

full_ts() ->
    {Megasecs, Secs, Microsecs} = now(),
    Microsecs + 1000000 * (Secs + 1000000 * Megasecs).


cancel_timer(Timer) ->
    case Timer of
        undefined ->
            false;
        _ ->
            erlang:cancel_timer(Timer)
    end.

wait_until(Fun) when is_function(Fun) ->
    MaxTime = 1000,
    Delay = 100,
    Retry = MaxTime div Delay,
    wait_until(Fun, Retry, Delay).

%% @doc Retry `Fun' until it returns `Retry' times, waiting `Delay'
%% milliseconds between retries. This is our eventual consistency bread
%% and butter
wait_until(_, 0, _) ->
    fail;

wait_until(Fun, Retry, Delay) when Retry > 0 ->
    Pass = Fun(),
    case Pass of
        true ->
            ok;
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry-1, Delay)
    end.

is_pid_alive(Pid) when is_pid(Pid) =:= false ->
    false;

is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);

is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
        false ->
            false;
        true ->
            case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                true ->
                    true;
                false ->
                    false;
                {badrpc, _Reason} ->
                    false
            end
    end.
