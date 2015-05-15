-module(mastermind).
-export([new/0, new/1, new/3, random_code/0, size/1, make_guess/1,
	 compute_score/2]).
-compile({no_auto_import, [size/1]}).

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
    [white, yellow, pink, purple, orange]. %, turquoise].

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
    spud:sample(Codes).

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
    first_guess(This),
    halt(0).

first_guess(This) ->
    %% For the first guess we only need to determine what category
    %% of guess is best: all the same color, 3 of one color and 1 of
    %% another, etc.  The actual colors and positions don't matter.
    %% So we caategorize all the codes, use the first one as a representative
    %% to get the worst case path length, then pick a random guess
    %% from the best category.
    %%
    %% It turns out that all categories of guess for four code positions
    %% and six colors have a worst-case papth length of 5.  So we can
    %% just pick a guess at random.
    %%
    %% We might want to see if some type of guess has a shorter path length
    %% on average.
    %%
    Codes = This#mastermind.codes,
    Categorized = group_by(Codes, fun (Code) -> get_category(Code) end),
    lists:foreach(
      fun (E) ->
	      {_Category, List} = E,
	      Guess = hd(List),
	      PathLengths = path_lengths(This, Guess),
	      {Paths, TotalPathLength} = PathLengths,
	      io:format("~p: ~p => ~p~n",
			[_Category, PathLengths, TotalPathLength / Paths])
      end,
      Categorized).

path_lengths(This, Guess) ->
    %% Here we don't get to choose the score, we just get what we get.
    %% So we need to consider the average path length over all scores.
    spud:mapreduce(
      This#mastermind.all_scores,
      fun (Score) ->
	      New = new(This, Guess, Score),
	      case size(New) of
		  0 ->
		      %% This Score is not possible for this Guess, so
		      %% it doesn't contribute to path lengths.
		      {0, 0};
		  1 ->
		      {1, 1};
		  _ ->
		      {N, L} = path_lengths(New),
		      {N, L + N}  % Effectively add 1 to each path length.
	      end
      end,
      fun ({Paths, TotalLength}, {APaths, ATotalLength}) ->
	      {APaths + Paths, ATotalLength + TotalLength}
      end,
      {0, 0}).

path_lengths(This) ->
    %% We choose the guess.  We want the guess with the shortest
    %% average path length.
    Codes = codes_to_try(This),
    case length(Codes) < 15 of
	true ->
	    F = fun spud:min/2;
	false ->
	    F = fun (List, Func) ->
			spud:parallel_min(limiter, List, Func)
		end
    end,
    {_Average, Count, TotalLength} =
	F(Codes,
	  fun (Guess) ->
		  {Count, TotalLength} = path_lengths(This, Guess),
		  {TotalLength / Count, Count, TotalLength}
	  end),
    {Count, TotalLength}.

compute_score(Code, Guess) ->
    Zipped = lists:zip(Code, Guess),
    {Hits, Misses} = lists:partition(
		       fun ({A, A}) ->
			       true;
			   (_) ->
			       false
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
