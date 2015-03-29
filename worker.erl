-module(worker).
-behavior(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, run/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% Starts a pool worker and returns {ok, Pid}.
%%
start() ->
    gen_server:start(?MODULE, [], []).

%% Tells the worker to run Func.
%%
run(Func) ->
    gen_server:cast({run, Func}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, void}.

handle_call(not_supported, _From, _State) ->
    not_supported.

handle_cast({run, Func}, State) ->
    Func(),
    {noreply, State}.

handle_info(not_supported, _State) ->
    not_supported.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


