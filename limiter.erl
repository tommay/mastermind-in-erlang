-module(limiter).
-behavior(gen_server).

-record(limiter, {permits}).

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

%% Starts a semaphore process and registers under the given name.
%%
start(Name, Permits) ->
    gen_server:start({local, Name}, ?MODULE, Permits, []).

%% Spawns a process to run Func if there is a permit available, else
%% runs Func in-process.
%%
run(Limiter, Func) ->
    case gen_server:call(Limiter, {run, Func}) of
	true ->
	    ok;
	false ->
	    Func()
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Permits) ->
    {ok, #limiter{permits = Permits}}.

%% No permits.
%%
handle_call({run, _Func}, _From, State = #limiter{permits = 0}) ->
    {reply, false, State};

%% There is a permit.
%%
handle_call({run, Func}, _From, State = #limiter{permits = Permits}) ->
    monitor(process, spawn(Func)),
    {reply, true, State#limiter{permits = Permits - 1}}.

handle_cast(_, State) ->
    {noreply, State}.

%% A worker exited.  Increment permits.
%%
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State = #limiter{permits = Permits}) ->
    {noreply, State#limiter{permits = Permits + 1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
