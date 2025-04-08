-module(master).

-export([start/3, stop/1, log_guess/4, ordMap/0]).

ordMap() ->
    #{1 => {running, 96, 8},
      2 => {running, 87, 7},
      3 => {running, 87, 8},
      4 => {winner, 82, 8},
      5 => {running, 78, 7}}.

init() ->
    maps:new().

%% @doc Starts the server and `NumWorkers' workers. The server is started with a
%% random secret number between `Min' and `Max'.

-spec start(NumWorkers, Min, Max) -> Master
    when NumWorkers :: integer(),
         Min :: integer(),
         Max :: integer(),
         Master :: pid().
start(NumWorkers, Min, Max) ->
    Secret = utils:random(Min, Max),
    Server = server:start(Secret),
    Master = spawn(fun() -> loop(NumWorkers, init()) end),

    Workers =
        maps:from_list([{Pid,
                         #{numberOfGuesses => 0,
                           lastGuess => na,
                           status => running}}
                        || Pid
                               <- [worker:start(Server, Master, Min, Max)
                                   || _ <- lists:seq(1, NumWorkers)]]),

    Master ! {workers, Workers},

    Master.

%% @doc Stops the `Master'.

-spec stop(Master) -> stop when Master :: pid().
stop(Master) ->
    Master ! stop.

loop(0, Map) ->
    io:format("DONE ~p~n", [Map]);
loop(CountDown, Map) ->
    receive
        {workers, Workers} ->
            loop(CountDown, Workers);
        {right, Guess, From} ->
            io:format("worker ~p reported that it was correct with the guess ~p~n", [From, Guess]),

            LoserList =
                [{X, {loser, B, C}}
                 || {X, {_, B, C}}
                        <- maps:to_list(Map)], % Make a list of all our workers and make them all losers

            LoserMap =
                maps:from_list(LoserList), % Convert the list with every worker marked as a loser back into a map

            {ok, Winner} = maps:find(From, Map), % Save the winner's data in Winner

            {_, LastGuess, NumberOfGuesses} =
                Winner, % Extract the data out of the tuple using pattern matching

            UpdatedMap =
                maps:update(From,
                            {winner, LastGuess, NumberOfGuesses},
                            LoserMap), % Update the map with the correct worker marked as winner together with their last guess and number of guesses

            % Use list comprehension to get all Pids and send an 'EXIT' message to all where PID does not equal our winner
            [Pid ! {'EXIT', self(), loser} || Pid <- maps:keys(Map), Pid =/= From],

            io:format("Statistics: ~n~p~n", [UpdatedMap]);
        % loop(CountDown, UpdatedMap)
        {guess, From, Guess, NumberOfGuesses} ->
            UpdatedMap = maps:update(From, {running, Guess, NumberOfGuesses}, Map),
            loop(CountDown, UpdatedMap);
        print ->
            io:format("~p~n", [Map]),
            loop(CountDown, Map);
        stop ->
            ok;
        Msg ->
            io:format("master:loop/2 Unknown message ~p~n", [Msg]),
            loop(CountDown, Map)
    end.

-spec log_guess(Master, From, Guess, NumberOfGuesses) -> ok
    when Master :: pid(),
         From :: pid(),
         Guess :: number(),
         NumberOfGuesses :: number().
log_guess(Master, From, Guess, NumberOfGuesses) ->
    Master ! {guess, From, Guess, NumberOfGuesses},
    ok.
