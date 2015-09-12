-module(log).
-export([start/0, indent/0, outdent/0, log/2]).

start() ->
    Pid = spawn(fun () -> loop(0) end),
    register(logger, Pid).

indent() ->
    logger ! indent.

outdent() ->
    logger ! outdent.

log(Format, Args) ->
    logger ! {Format, Args}.

loop(Indent) ->
    receive
	indent ->
	    loop(Indent + 2);
	outdent ->
	    loop(Indent - 2);
	{Format, Args} ->
	    io:format("~s", [lists:flatten(lists:duplicate(Indent, " "))]),
	    io:format(Format, Args),
	    loop(Indent)
    end.
