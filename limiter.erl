-module(limiter).
-export([start/2, run/2]).

-define(RUN, run).
-define(RESULT, result).

%% Starts a semaphore process and registers under the given name.
%%
start(Name, Permits) ->
    Pid = spawn(fun () -> loop(Name, Permits) end),
    register(Name, Pid).

%% Spawns a process to run Func if there is a permit available, else
%% runs Func in-process.
%%
run(Name, Func) ->
    Name ! {self(), ?RUN, Func},
    receive
	{Name, ?RESULT, true} ->
	    ok;
	{Name, ?RESULT, false} ->
	    Func()
    end.

loop(Name, Permits) ->
    receive
	{Pid, ?RUN, Func} ->
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
