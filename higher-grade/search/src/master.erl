-module(master).

-export([start/3, stop/1, log_guess/4]).

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
            % io:format("Master received workers map: ~p~n", [Workers]),
            loop(CountDown, Workers);
        {right, Guess, From} ->
            io:format("worker ~p reported that it was correct with the guess ~p~n", [From, Guess]),
            {ok, Winner} = maps:find(From, Map),

            {_, LastGuess, NumberOfGuesses} = Winner,

            UpdatedMap = maps:update(From, {winner, LastGuess, NumberOfGuesses}, Map),

            % Use list comprehension to get all Pids and send an 'EXIT' message to all where PID does not equal our winner
            [Pid ! {'EXIT', self(), loser} || Pid <- maps:keys(Map), Pid =/= From],

            io:format("Statistics: ~n~p~n", [UpdatedMap]),
            loop(CountDown, UpdatedMap);
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
