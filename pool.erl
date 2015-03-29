-module(pool).
-behavior(gen_server).

-record(pool, {size, idle_workers = [], busy_count = 0}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2, run/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% Starts a process pool of the given Size.
%%
start(Name, Size) ->
    gen_server:start({local, Name}, ?MODULE, Size, []).

%% Try to run Func in a pool worker, and if none are available then
%% run it in-process.
%%
run(Pool, Func) ->
    case gen_server:call(Pool, {run, Func}) of
	true ->
	    ok;
	false ->
	    Func()
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Size) ->
    {ok, #pool{size = Size}}.

%% There is an idle worker.  Tell it to run Func.
%% XXX This just assumes the worker han't crashed.
%%
handle_call({run, Func}, _From, State = #pool{idle_workers = [Worker | T], busy_count = BusyCount}) ->
    Me = self(),
    FuncAndDone = fun () ->
			  Func(),
			  gen_server:cast(Me, {done, Worker})
		  end,
    gen_server:cast(Worker, {run, FuncAndDone}),
    {reply, true, State#pool{idle_workers = T, busy_count = BusyCount + 1}};

%% No idle workers, but we can add a new worker to the pool and keep trying.
%%
handle_call(Arg = {run, _Func}, From, State = #pool{size = Size, idle_workers = [], busy_count = BusyCount}) when BusyCount < Size ->
    {ok, Worker} = worker:start(),
    handle_call(Arg, From, State#pool{idle_workers = [Worker]});

%% No idle workers.  If this matches then the pool is full so return false.
%%
handle_call({run, _Func}, _From, State = #pool{idle_workers = []}) ->
    {reply, false, State}.

%% A worker finished.  Add it to idle_workers and decrement busy_count.
%%
handle_cast({done, Pid}, State = #pool{idle_workers = IdleWorkers, busy_count = BusyCount}) ->
    {noreply, State#pool{idle_workers = [Pid | IdleWorkers], busy_count = BusyCount - 1}}.

%% A worker exited.  Decrement busy_count.
%%
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State = #pool{busy_count = BusyCount}) ->
    {noreply, State#pool{busy_count = BusyCount - 1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
