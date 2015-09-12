-module(pool).
-export([start/2, run/2]).

-define(TRY_RUN, try_run).
-define(DONE, done).
-define(RESULT, result).

%% Starts a process pool and registers under the given name.
%%
start(Name, Size) ->
    Pid = spawn(fun () ->
			process_flag(trap_exit, true),
			loop(Name, Size, [], 0)
		end),
    register(Name, Pid).

%% Try to run Func in a pool worker, and if none are available then
%% run it in-process.
%%
run(Name, Func) ->
    Name ! {self(), ?TRY_RUN, Func},
    receive
	{Name, ?RESULT, true} ->
	    ok;
	{Name, ?RESULT, false} ->
	    Func()
    end.

loop(Name, Size, IdleWorkers = [Worker|T], BusyCount) ->
    receive
	{Pid, ?TRY_RUN, Func} ->
	    Worker ! {run, Func, self()},
	    Pid ! {Name, ?RESULT, true},
	    loop(Name, Size, T, BusyCount + 1);
	Done ->
	    handle_done(Name, Size, IdleWorkers, BusyCount, Done)
    end;
loop(Name, Size, _IdleWorkers = [], BusyCount) when BusyCount < Size ->
    loop(Name, Size, [spawn_link(fun worker/0)], BusyCount);
loop(Name, Size, IdleWorkers = [], BusyCount) ->
    receive
	{Pid, ?TRY_RUN, _Func} ->
	    Pid ! {Name, ?RESULT, false},
	    loop(Name, Size, IdleWorkers, BusyCount);
	Done ->
	    handle_done(Name, Size, IdleWorkers, BusyCount, Done)
    end.

handle_done(Name, Size, IdleWorkers, BusyCount, Done) ->
    case Done of
	{?DONE, Pid} ->
	    loop(Name, Size, [Pid | IdleWorkers], BusyCount - 1);
	{'EXIT', _Pid, _Reason} ->
	    loop(Name, Size, IdleWorkers, BusyCount - 1)
    end.

worker() ->
    receive
	{run, Func, Pid} ->
	    Func(),
	    Pid ! {?DONE, self()}
    end,
    worker().
