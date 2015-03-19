-module(limiter).
-export([start/2, try_spawn/2]).

-define(TRY_SPAWN, try_spawn).
-define(RESULT, result).

%% Starts a semaphore process and registers under the given name.
%%
start(Name, Permits) ->
    Pid = spawn(fun () -> loop(Name, Permits) end),
    register(Name, Pid).

%% Non-blocking call to try to spawn a process to call Func.  If a permit
%% is available then a process will be spawned, otherwise it won't.
%%
try_spawn(Name, Func) ->
    Name ! {self(), ?TRY_SPAWN, Func},
    receive
	{Name, ?RESULT, Result} ->
	    Result
    end.

loop(Name, Permits) ->
    receive
	{Pid, ?TRY_SPAWN, Func} ->
	    case Permits == 0 of
		true ->
		    Pid ! {Name, ?RESULT, false},
		    loop(Name, Permits);
		false ->
		    Pid ! {Name, ?RESULT, true},
		    monitor(process, spawn(Func)),
		    loop(Name, Permits - 1)
	    end;
	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    loop(Name, Permits + 1)
    end.
