-module(worker).

-export([start/4]).

%% @doc Starts a worker process. The worker will make random guesses between
%% `Min' and `Max'.

-spec start(Server, Master, Min, Max) -> Worker
    when Server :: pid(),
         Master :: pid(),
         Min :: number(),
         Max :: number(),
         Worker :: pid().
start(Server, Master, Min, Max) ->
    %    io:format("I am ~p and just spawned",self()),
    spawn(fun() -> loop(Server, Master, Min, Max, 0) end).

loop(Server, Master, Min, Max, Guesses) ->
    process_flag(trap_exit, true),
    Guess = utils:random(Min, Max),
    Server ! {guess, Guess, self()},
    master:log_guess(Master, self(), Guess, Guesses + 1),

    receive
        {right, Guess} ->
            Master ! {right, Guess, self()},
            io:format("~p ~*.. B <=== FOUND IT :-> ~n", [self(), utils:width(Max), Guess]);
        {wrong, Guess} ->
            io:format("~p ~*.. B~n", [self(), utils:width(Max), Guess]),
            loop(Server, Master, Min, Max, Guesses + 1);
        {'EXIT', _From, loser} ->
            io:format("~p I lose :(~n", [self()])
    end.
