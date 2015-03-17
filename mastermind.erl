-module(mastermind).
-export([new/0, new/1, new/3, random_code/0, size/1, make_guess/1,
	 compute_score/2]).
-compile({no_auto_import, [size/1]}).
-compile({no_auto_import, [min/2]}).
-compile({no_auto_import, [max/2]}).
-compile(export_all).

-record(mastermind, {try_all_codes = false, codes, all_codes, all_scores}).

new() ->
    new(false).

%% try_all_codes: true means make guesses from CODES which contains
%% all possible codes.  It remains to be seen whether having a larger
%% set of guesses makes solving faster, i.e., fewer guesses.  False
%% means make guesses only from codes.  codes: an Array of all codes
%% that are possible given the previous guesses and scores.  Tests on
%% one-ply searches show using all codes on average takes more
%% guesses.
%%
new(TryAllCodes) ->
    AllCodes = codes(),
    AllScores = scores(),
    #mastermind{try_all_codes = TryAllCodes, codes = AllCodes,
		all_codes = AllCodes, all_scores = AllScores}.

new(This, Guess, Score) ->
    This#mastermind{codes = filter_codes(This#mastermind.codes, Guess, Score)}.

colors() ->
    [white, yellow, pink, purple, orange, turquoise].

codes() ->
    spud:combinations(4, colors()).

%% Create all possible scores, i.e., different numbers of red/white.
%% Scores are represented as a tuple of numbers {R, W} where R and W
%% are between 0 and 4.  The score {3, 1} is not possible.
%%
scores() ->
    [{R, W} || R <- lists:seq(0, 4), W <- lists:seq(0, 4-R),
	       not ((R == 3) and (W == 1))].

random_code() ->
    random_code(codes()).

random_code(Codes) ->
    random:seed(now()),
    sample(Codes).

sample(List) ->
    lists:nth(random:uniform(length(List)), List).

size(This) ->
    length(This#mastermind.codes).

codes_to_try(This) ->
    case This#mastermind.try_all_codes of
	true ->
	    This#mastermind.all_codes;
	false ->
	    This#mastermind.codes
    end.

make_guess(This) ->
    case size(This) == length(This#mastermind.all_codes) of
	true ->
	    %% This saves time on the first guess.
	    first_guess(This);
	false ->
	    best_guess(This)
    end.

first_guess(This) ->
    %% For the first guess we only need to determine what category
    %% of guess is best: all the same color, 3 of one color and 1 of
    %% another, etc.  The actual colors and positions don't matter.
    %% So we caategorize all the codes, use the first one as a representative
    %% to get the worst case path length, then pick a random guess
    %% from the best category.
    Codes = This#mastermind.codes,
    Categorized = group_by(Codes, fun (Code) -> get_category(Code) end),
    {{_Category, List}, _Min} =
	parallel_min_by(
	  Categorized,
	  fun (E) ->
		  {_Category, List} = E,
		  Guess = hd(List),
		  {worst_case_path_length(This, Guess), random:uniform()}
	  end),
    sample(List).

best_guess(#mastermind{codes = [H]}) ->
    %% This case is only necessary if we're making guesses from
    %% all_codes instead of codes.
    H;
best_guess(This) ->
    %% Find the longest path to finish for each code.  Return the code
    %% with the shortest worst-case path.
    {Guess, {PathLength, _}} =
    parallel_min_by(
      codes_to_try(This),
      fun (Guess) ->
	      %% Include a random number to mix things up when there's a tie.
	      random:seed(now()),
	      {worst_case_path_length(This, Guess), random:uniform()}
      end),
    io:format("~p (~p)~n", [Guess, PathLength]),
    Guess.

worst_case_path_length(This, Guess) ->
    max(This#mastermind.all_scores,
	fun (Score) ->
		New = new(This, Guess, Score),
		case size(New) of
		    0 ->
			%% This Score is not possible for this Guess,
			%% so don't include it in the max.
			-1;
		    1 ->
			1;
		    _ ->
			1 + path_length(New)
		end
	end).

path_length(This) ->
    min(
      codes_to_try(This),
      fun (Guess) ->
	      worst_case_path_length(This, Guess)
      end).

%% The parallel_max version works fine, but erl seems to choke when
%% the number of processes gets too high.  1296 codes times 14 scores
%% == 18144 processes, althouth some of them should be finished by the
%% time they're all started.
%%
parallel_worst_case(This, Guess) ->
    parallel_max(
      This#mastermind.all_scores,
      fun (Score) ->
	      size(new(This, Guess, Score))
      end).

compute_score(Code, Guess) ->
    Zipped = lists:zip(Code, Guess),
    {Hits, Misses} = lists:partition(
		       fun (Z) ->
			       element(1, Z) == element(2, Z)
		       end,
		       Zipped),
    Red = length(Hits),
    {Code2, Guess2} = lists:unzip(Misses),
    Misses2 = Code2 -- Guess2,
    White = length(Code2) - length(Misses2),
    {Red, White}.

%% Return the codes for which the given guess gets the given score.
%%
filter_codes(Codes, Guess, Score) ->
    [Code || Code <- Codes, compute_score(Code, Guess) == Score].

parallel_min_by(List, Func) ->
    {Min, Result} = parallel_min(
		    List,
		    fun (Element) ->
			    {Func(Element), Element}
		    end),
    {Result, Min}.

parallel_min(List, Func) ->
    parallel_minmax(List, Func, fun (Value, Result) -> Value < Result end).
					
parallel_max(List, Func) ->
    parallel_minmax(List, Func, fun (Value, Result) -> Value > Result end).
					
parallel_minmax(List, Func, Pred) ->
    Self = self(),
    Ref = make_ref(),
    lists:foreach(
      fun (Element) ->
	      spawn(fun () -> Self ! {Ref, Func(Element)} end)
      end,
      List),
    collect_minmax(Ref, Pred, length(List)).

collect_minmax(Ref, Pred, N) ->
    collect_minmax(Ref, Pred, N, undefined).

collect_minmax(_Ref, _Pred, 0, Result) ->
    Result;
collect_minmax(Ref, Pred, N, Result) ->
    receive
	{Ref, Value} ->
	    case (Result == undefined) orelse Pred(Value, Result) of
		true ->
		    collect_minmax(Ref, Pred, N - 1, Value);
		false ->
		    collect_minmax(Ref, Pred, N - 1, Result)
	    end
    end.

min_by(List, Func) ->
    {Min, Result} = min(List, fun (E) -> {Func(E), E} end),
    {Result, Min}.

min(List, Func) ->
    min(List, Func, undefined).

min([], _Func, Result) ->
    Result;
min([H|T], Func, Result) ->
    Value = Func(H),
    case (Result == undefined) orelse (Value < Result) of
	true->
	    min(T, Func, Value);
	false ->
	    min(T, Func, Result)
    end.


max(List, Func) ->
    max(List, Func, undefined).

max([], _Func, Result) ->
    Result;
max([H|T], Func, Result) ->
    Value = Func(H),
    case (Result == undefined) orelse (Value > Result) of
	true ->
	    max(T, Func, Value);
	false ->
	    max(T, Func, Result)
    end.

get_category(Code) ->
    Uniq = spud:uniq(Code),
    Counts = [length([C || C <- Code, C == U]) || U <- Uniq],
    lists:sort(Counts).

%% -> [{key1,[val, val, ...]}, {key1,[val, val, ...]}, ...]
%%
group_by(List, Func) ->
    group_by(List, Func, dict:new()).

group_by([], _Func, Dict) ->
    dict:to_list(Dict);
group_by([H|T], Func, Dict) ->
    Group = Func(H),
    group_by(T, Func, dict:append(Group, H, Dict)).
