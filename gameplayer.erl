-module(gameplayer).
-compile(export_all).

-record(gameplayer, {use_all_codes = true}).

%% gameplayer:play_a_game(gameplayer:new(true)).
%% gameplayer:play_games(gameplayer:new(false), 10).
%%
new(UseAllCodes) ->
    #gameplayer{use_all_codes = UseAllCodes}.

play_games(UseAllCodes, N) ->
    Gameplayer = new(UseAllCodes),
    Turns = [play_a_game(Gameplayer) || _ <- lists:seq(1, N)],
    io:format("~n"),
    io:format("~p~n", [lists:sum(Turns) / N]),
    io:format("~p~n", [lists:min(Turns)]),
    io:format("~p~n", [lists:max(Turns)]).

play_a_game(This) ->
    io:format("."),
    Game = mastermind:new(This#gameplayer.use_all_codes),
    Code = mastermind:random_code(),
    play_a_game(Game, Code, 1).

play_a_game(Game, Code, Turns) ->
    Guess = mastermind:make_guess(Game),
    case Guess == Code of
	true ->
	    Turns;
	false ->
	    Score = mastermind:compute_score(Code, Guess),
	    Game2 = mastermind:new(Game, Guess, Score),
	    play_a_game(Game2, Code, Turns + 1)
    end.