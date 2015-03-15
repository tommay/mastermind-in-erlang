-module(spud).
-export([min_by/2, slices/2, array_min_by/2, array_update/3, uniq/1]).
-export([tuple_update/3, tuple_min_by/2]).
-export([combinations/2]).
-export([format/2, debug/1, debug/2]).

%% Some handy utility functions.

%% Returns the element of List with the minimum value computed by Func.
%%
min_by(List, Func) when is_list(List), is_function(Func) ->
    %% This creates an entire new list.  Could use foldl to avoid that
    %% and just remember the min on the fly.  But this is simple.
    Tuples = [{Func(Element), Element} || Element <- List],
    {_, MinElement} = lists:min(Tuples),
    MinElement.

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

%% You'd think there would be an array:update like dict:update so the
%% structure would only have to be descended once, but no, it takes two
%% calls to do an update.
%%
array_update(Index, Func, Array) ->
    Value = array:get(Index, Array),
    array:set(Index, Func(Value), Array).

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
