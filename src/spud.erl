-module(spud).
-compile({no_auto_import,[min/2]}).
-compile({no_auto_import,[max/2]}).
-export([sample/1, slices/2, uniq/1]).
-export([array_update/3, tuple_update/3]).
-export([combinations/2]).
-export([mapreduce/4, parallel_mapreduce/4, parallel_mapreduce/5]).
-export([min/2, max/2, min_by/2, max_by/2]).
-export([parallel_min/2, parallel_max/2, parallel_min/3, parallel_max/3]).
-export([parallel_min_by/2, parallel_max_by/2]).
-export([parallel_min_by/3, parallel_max_by/3]).
-export([array_min_by/2, tuple_min_by/2]).
-export([format/2, debug/1, debug/2]).

%% Some handy utility functions.

%% Returns a random sample from the List, using random:uniform/1 which
%% uses the process dictionary for state.
%%
sample(List) ->
    lists:nth(random:uniform(length(List)), List).

%% Returns a list containing elements each containing N elements from
%% the original list.
%%
slices([], _N) when is_integer(_N) ->
    [];
slices(List, N) ->
    {Slice, Rest} = lists:split(N, List),
    [Slice | slices(Rest, N)].

%% Returns the list with duplicate elements removed.
%%
uniq(List) when is_list(List) ->
    sets:to_list(sets:from_list(List)).

%% Returns a new Array with the Indexth value updated by calling Func
%% on it.  Index starts at 0.
%% You'd think there would be an array:update like dict:update so the
%% structure would only have to be descended once, but no, it takes two
%% calls to do an update.
%%
array_update(Index, Func, Array) ->
    Value = array:get(Index, Array),
    array:set(Index, Func(Value), Array).

%% Returns a new Tuple with the Indexth value updated by calling Func
%% on it.  Index starts at 0.
%%
tuple_update(Index, Func, Tuple) ->
    Value = element(Index + 1, Tuple),
    setelement(Index + 1, Tuple, Func(Value)).

%% Returns all combinations of length N of the elements in List.  Each
%% combination will use each element zero to N times.  If N is zero
%% then the result is [[]] because there is one combination with zero
%% elements and it is empty.
%%
combinations(N, List) when is_integer(N), is_list(List) ->
    C = combinations(N, [[]], List),
    %% Reverse each combination because things look better that way.
    [lists:reverse(E) || E <- C].

combinations(0, Accum, _List) ->
    Accum;
combinations(N, Accum, List) ->
    NewAccum = lists:flatmap(
	    fun (C) ->
		    [[E | C] || E <- List]
	    end,
	    Accum),
    combinations(N - 1, NewAccum, List).

%% Performs a mapreduce on the List and returns the result.  All computations
%% are done in-process.
%%
mapreduce([], _Map, _Reduce, Accum) ->
    Accum;
mapreduce([H|T], Map, Reduce, Accum) ->
    mapreduce(T, Map, Reduce, Reduce(Map(H), Accum)).

%% Performs a mapreduce on the List and returns the result.  Map operations
%% are spawned, the reduce is done in-process.
%%
parallel_mapreduce(List, Map, Reduce, Accum) ->
    limited_mapreduce(fun spawn/1, List, Map, Reduce, Accum).

%% Performs a mapreduce on the List and returns the result.  Map
%% operations are spawned but are limited by the given Limiter so we
%% don't spawn too many.  If we can't spawn a map process then the map
%% is performed in-process.
%%
parallel_mapreduce(Limiter, List, Map, Reduce, Accum)->
    Run = fun (Func) ->
		  limiter:run(Limiter, Func)
	  end,
    limited_mapreduce(Run, List, Map, Reduce, Accum).

limited_mapreduce(Run, List, Map, Reduce, Accum) ->
    Self = self(),
    Ref = make_ref(),
    lists:foreach(
      fun (Element) ->
	      Run(fun () -> Self ! {Ref, Map(Element)} end)
      end,
      List),
    collect_mapreduce(Ref, Reduce, length(List), Accum).

collect_mapreduce(_Ref, _Reduce, 0, Accum) ->
    Accum;
collect_mapreduce(Ref, Reduce, N, Accum) ->
    receive
	{Ref, Value} ->
	    collect_mapreduce(Ref, Reduce, N - 1, Reduce(Value, Accum))
    end.

%% Applies Func to each value in the List and returns the minimum.
%%
min(List, Func) ->
    mapreduce(List, Func, fun min_reduce/2, undefined).

%% Applies Func to each value in the List and returns the maximum.
%%
max(List, Func) ->
    mapreduce(List, Func, fun max_reduce/2, undefined).

min_reduce(Value, undefined) ->
    Value;
min_reduce(Value, Accum) when Value < Accum ->
    Value;
min_reduce(_Value, Accum) ->
    Accum.

max_reduce(Value, undefined) ->
    Value;
max_reduce(Value, Accum) when Value > Accum ->
    Value;
max_reduce(_Value, Accum) ->
    Accum.

%% Returns the element of List with the minimum value computed by Func.
%%
min_by(List, Func) ->
    {Min, Result} = min(List, fun (E) -> {Func(E), E} end),
    {Result, Min}.

%% Returns the element of List with the maximum value computed by Func.
%%
max_by(List, Func) ->
    {Max, Result} = max(List, fun (E) -> {Func(E), E} end),
    {Result, Max}.

%% Applies Func to each value in the List and returns the minimum.  Spawns
%% a new process for each value to compute Func(Value).
%%
parallel_min(List, Func) ->
    parallel_mapreduce(List, Func, fun min_reduce/2, undefined).
					
%% Applies Func to each value in the List and returns the minimum.  A
%% new processs is spawned for each value to compute Func(Value) if
%% the limiter allows it, otherwise Func(Value) is computed
%% in-process.
%%
parallel_min(Limiter, List, Func) ->
    parallel_mapreduce(Limiter, List, Func, fun min_reduce/2, undefined).
					
%% Applies Func to each value in the List and returns the maximum.  Spawns
%% a new process for each value to compute Func(Value).
%%
parallel_max(List, Func) ->
    parallel_mapreduce(List, Func, fun max_reduce/2, undefined).
					
%% Applies Func to each value in the List and returns the maximum.  A
%% new processs is spawned for each value to compute Func(Value) if
%% the limiter allows it, otherwise Func(Value) is computed
%% in-process.
%%
parallel_max(Limiter, List, Func) ->
    parallel_mapreduce(Limiter, List, Func, fun max_reduce/2, undefined).

%% Returns the element of List with the minimum value computed by
%% Func.  Spawns a new process for each value to compute Func(Value).
%%
parallel_min_by(List, Func) ->
    {Min, Result} = parallel_min(
		      List,
		      fun (Element) ->
			      {Func(Element), Element}
		      end),
    {Result, Min}.

%% Returns the element of List with the minimum value computed by
%% Func.  A new processs is spawned for each value to compute
%% Func(Value) if the limiter allows it, otherwise Func(Value) is
%% computed in-process.
%%
parallel_min_by(Limiter, List, Func) ->
    {Min, Result} = parallel_min(
		      Limiter,
		      List,
		      fun (Element) ->
			      {Func(Element), Element}
		      end),
    {Result, Min}.

%% Returns the element of List with the maximum value computed by
%% Func.  Spawns a new process for each value to compute Func(Value).
%%
parallel_max_by(List, Func) ->
    {Max, Result} = parallel_max(
		      List,
		      fun (Element) ->
			      {Func(Element), Element}
		      end),
    {Result, Max}.

%% Returns the element of List with the minimum value computed by
%% Func.  A new processs is spawned for each value to compute
%% Func(Value) if the limiter allows it, otherwise Func(Value) is
%% computed in-process.
%%
parallel_max_by(Limiter, List, Func) ->
    {Max, Result} = parallel_max(
		      Limiter,
		      List,
		      fun (Element) ->
			      {Func(Element), Element}
		      end),
    {Result, Max}.

%% Returns the element of Array with the minimum value computed by Func.
%%
array_min_by(Array, Func) when is_function(Func) ->
    {_, Element} =
	array:foldl(
	  fun (_Index, Element, Accum = {MinN, _MinElement}) ->
		  N = Func(Element),
		  case MinN == undefined orelse N < MinN of
		      true -> {N, Element};
		      false -> Accum
		  end
	  end,
	  {undefined, undefined},
	  Array),
    Element.

%% Returns the element of Tuple with the minimum value computed by Func.
%%
tuple_min_by(Tuple, Func) when is_function(Func) ->
    {_, Element} =
	tuple_foldl(
	  fun (Element, Accum = {MinN, _MinElement}) ->
		  N = Func(Element),
		  case MinN == undefined orelse N < MinN of
		      true -> {N, Element};
		      false -> Accum
		  end
	  end,
	  {undefined, undefined},
	  Tuple),
    Element.

tuple_foldl(Func, Accum, Tuple) ->
    tuple_foldl(Func, Accum, Tuple, size(Tuple)).

tuple_foldl(_Func, Accum, _Tuple, N) when N =< 0 ->
    Accum;
tuple_foldl(Func, Accum, Tuple, N) ->
    tuple_foldl(Func, Func(element(N, Tuple), Accum), Tuple, N - 1).

%% Returns a string with Data formatted by Format.
%%
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%% Write some information for debugging.
%%
debug(Format, Data) ->
    debug(format(Format, Data)).

%% Write a string for debugging.
%%
debug(String) ->
%    String.
    io:format(String).
