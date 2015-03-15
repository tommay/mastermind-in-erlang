-module(mastermind).
-export([new/0, new/1, new/3, random_code/0, size/1, make_guess/1,
	 compute_score/2]).
-compile({no_auto_import, [size/1]}).

-record(mastermind, {use_all_codes = false, codes, all_codes, all_scores}).

new() ->
    new(false).

%% use_all_codes: true means make guesses from CODES which contains
%% all possible codes.  It remains to be seen whether having a larger
%% set of guesses makes solving faster, i.e., fewer guesses.  False
%% means make guesses only from codes.  codes: an Array of all codes
%% that are possible given the previous guesses and scores.  Tests on
%% one-ply searches show using all codes on average takes more
%% guesses.
%%
new(UseAllCodes) ->
    AllCodes = codes(),
    %% Create all valid scores, i.e., different numbers of red/white.
    %% Scores are represented as a tuple of numbers {R, W} where R and
    %% W are between 0 and 4.
    AllScores = [{R, W} || R <- lists:seq(0, 4), W <- lists:seq(0, 4-R)],
    #mastermind{use_all_codes = UseAllCodes, codes = AllCodes,
		all_codes = AllCodes, all_scores = AllScores}.

new(This, Guess, Score) ->
    This#mastermind{codes = filter_codes(This#mastermind.codes, Guess, Score)}.

colors() ->
    [white, yellow, pink, purple, orange, turquoise].

codes() ->
    spud:combinations(4, colors()).

random_code() ->
    random_code(codes()).

random_code(Codes) ->
    random:seed(now()),
    lists:nth(random:uniform(length(Codes)), Codes).

size(This) ->
    length(This#mastermind.codes).

make_guess(This) ->
    case size(This) == length(This#mastermind.all_codes) of
	true ->
	    %% This saves time on the first guess.  It's unknown
	    %% whether some guesses might be better, e.g., guesses
	    %% with more or less duplicate colors.
	    random_code(This#mastermind.codes);
	false ->
	    best_guess(This)
    end.

best_guess(#mastermind{codes = [H]}) ->
    %% This case is only necessary if we're making guesses from
    %% all_codes instead of codes.
    H;
best_guess(This) ->
    %% This chooses the guess that gives us (for the worst case score)
    %% the fewest possibile codes for the next round.  Choosing a
    %% guess from all possible codes may narrow down the possibilities
    %% later.  This is a one-ply search.

    Codes = case This#mastermind.use_all_codes of
		true ->
		    This#mastermind.all_codes;
		false ->
		    This#mastermind.codes
	    end,

    spud:min_by(
      Codes,
      fun (Guess) ->
	      lists:max(
		[size(new(This, Guess, Score)) ||
		    Score <- This#mastermind.all_scores])
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
