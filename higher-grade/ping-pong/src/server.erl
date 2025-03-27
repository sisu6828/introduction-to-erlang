%% @doc A server that keeps track of  <a target="_blank"
%% href="https://www.rd.com/culture/ablaut-reduplication/">ablaut
%% reduplication</a> pairs. You should implement two versions of the server. One
%% stateless server and one stateful server.
%%
%% <ul>
%% <li>
%% The stateless server keeps
%% track of a static number of ablaut reduplication pairs. Each pair is handled
%% by a separate message receive pattern.
%% </li>
%% <li>
%% The stateful server keeps
%% track of dynamic number of ablaut reduplication pairs using a <a
%% target="_blank" href="https://erlang.org/doc/man/maps.html">Map</a>.
%% </li>
%% </ul>
%% <p>
%% You should also implement process supervision of the server.
%% <ul>
%% <li>
%% The supervisor should <a target="_blank"
%% href="https://erlang.org/doc/reference_manual/processes.html#registered-processes">register</a>
%% the server process under the name `server'.
%% </li>
%% <li>
%% The name of a registered process can be used instead of the Pid when sending
%% messages to the process.
%% </li>
%% <li>
%% The supervisor should restart the server if the server terminates due to an
%% error.
%% </li>
%% </ul>
%% </p>

-module(server).

-export([start/2, update/0, update/1, stop/0, stop/1, loop_stateless/0, loop_statefull/1,
         start/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc The initial state of the stateful server.

-spec pairs() -> map().
pairs() ->
    #{ping => pong,
      tick => tock,
      hipp => hopp,
      ding => dong}.

%% @doc Starts the server.

-spec start(Stateful, Supervised) -> Server
    when Stateful :: boolean(),
         Supervised :: boolean(),
         Server :: pid().
start() ->
    start(true,true).

start(false, false) ->
    spawn(fun() -> loop_stateless() end);
start(false, true) ->
    spawn(fun() -> supervisor(false) end);
start(true, false) ->
    spawn(fun() -> loop_statefull(pairs()) end);
start(true, true) ->
    spawn(fun() -> supervisor(true) end).

%% @doc The server supervisor. The supervisor must trap exit, spawn the server
%% process, link to the server process and wait the server to terminate. If the
%% server terminates due to an error, the supervisor should make a recursive
%% call to it self to restart the server.

-spec supervisor(Stateful) -> ok when Stateful :: boolean().
supervisor(Stateful) ->
    io:format("Supervisor started~n"),
    io:format("Stateful:s value is : ~w~n", [Stateful]),
    case Stateful of
        true ->
            process_flag(trap_exit, true),
            Server = spawn_link(fun() -> loop_statefull(pairs()) end);
        false ->
            process_flag(trap_exit, true),
            Server = spawn_link(fun() -> loop_stateless() end)
    end,

    Server ! {alive, self()},
    receive
        {alive, X, POG} ->
            io:format("Server ~p ~w is alive~n", [X, POG])
    after 500 ->
        io:format("Server is not alive~n"),
        supervisor(Stateful)
    end,
    register(server, Server),
    ok,
    receive
        {'EXIT', PID, REASON} ->
            io:format("Server ~p terminated with reason: ~w!~n", [PID, REASON]),
            io:format("Supervisor restarting server...~n~n~n"),

            supervisor(Stateful);
        {Msg} ->
            io:format("supervisor/1: Unknown message: ~p~n", [Msg])
    end,
    io:format("Supervisor terminated, we should never get here~n").

%% @doc Terminates the supervised server.

-spec stop() -> ok | error.
stop() ->
    stop(server).

-spec stop(Server) -> ok | error when Server :: pid().
%% @doc Terminates the unsupervised server.

stop(Server) ->
    Server ! {stop, self()},
    receive
        {stop, ok} ->
            ok;
        Msg ->
            io:format("stop/1: Unknown message: ~p~n", [Msg]),
            error
    end.

%% @doc Makes the supervised server perform a hot code swap.

-spec update() -> ok | error.
update() ->
    update(server).

%% @doc Makes the unsupervised server perform a hot code swap.

-spec update(Server) -> ok | error when Server :: pid().
update(Server) ->
    Server ! {update, self()},
    receive
        {update, ok} ->
            ok;
        Msg ->
            io:format("update/1: Unknown message: ~p~n", [Msg]),
            error
    end.

%% @doc The process loop for the stateless server. The stateless server keeps
%% track of a static number of ablaut reduplication pairs. Each pair is handled
%% by a separate message receive pattern.

-spec loop_stateless() -> {stop, ok}.
loop_stateless() ->
    receive
        {ping, blipp, From} ->
            io:format("Stateless sever got a cursed blipp and will now die without "
                      "replying~n"),
            exit(simulated_bug),
            From ! {pong, blopp},
            loop_stateless();
        {ping, ding, From} ->
            From ! {pong, dong},
            loop_stateless();
        {ping, dddding, From} ->
            From ! {pong, dong},
            loop_stateless();
        {ping, ping, From} ->
            From ! {pong, pong},
            loop_stateless();
        {ping, king, From} ->
            From ! {pong, kong},
            loop_stateless();
        {ping, tick, From} ->
            From ! {pong, tock},
            loop_stateless();
        {stop, From} ->
            From ! {stop, ok},
            exit(stop);
        {update, From} ->
            %% TODO: Trigger a hot code swap.
            io:format("loop/0: Server says: Stateless Hot code swap~n"),
            From ! {update, ok},
            ?MODULE:loop_stateless();
        {alive, From} ->
            From ! {alive, stateless, self()},
            loop_stateless();
        Msg ->
            io:format("loop/0: Server says: Unknown message: ~p~n", [Msg]),
            loop_stateless()
    end.

%% @doc The process loop for the statefull server. The stateful server keeps
%% track of dynamic number of ablaut reduplication pairs using a <a
%% target="_blank" href="https://erlang.org/doc/man/Pairss.html">Map</a>.

-spec loop_statefull(Pairs) -> {stop, ok} when Pairs :: map().
loop_statefull(Pairs) ->
    receive
        {ping, flip, _From} ->
            exit(simulated_bug);
        {ping, Ping, From} ->
            Result = maps:find(Ping, Pairs),

            case Result of
                {ok, Value} ->
                    From ! {pong, Value},
                    loop_statefull(Pairs);
                error ->
                    From ! {pong, unknown},
                    loop_statefull(Pairs)
            end;
        {update, From} ->
            io:format("loop/1: Stateful Hot code swap~n"),
            From ! {update, ok},
            ?MODULE:loop_statefull(Pairs);
        {put, Ping, Pong, From} ->
            NewPairs = maps:put(Ping, Pong, Pairs),
            From ! {put, Ping, Pong, ok},
            loop_statefull(NewPairs);
        {alive, From} ->
            From ! {alive, statefull, self()},
            loop_statefull(Pairs);
        {stop, From} ->
            From ! {stop, ok},
            exit(stop),
            loop_statefull(Pairs);
        Msg ->
            io:format("loop/1: Unknown message: ~p~n", [Msg]),
            loop_statefull(Pairs)
    end.

%%%===================================================================
%%% Includes, defines, types and records
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%%  Test descriptions
%%====================================================================

%%====================================================================
%%  Setup and cleanup
%%====================================================================

%%====================================================================
%%  Unit tests

% server_create_test_() ->
%    [
%     ?_assertMatch(server:start() =/= undefined)
%    ].

% server_supervisor_test_() ->
%    [
%     ?_assertMatch(server:supervisor(true) =/= undefined),
%     ?_assertMatch(server:supervisor(false) =/= undefined)
%    ].

% server_stop_test_() ->
%    [
%     ?_assertMatch(server:stop() =/= undefined)
%    ].

%%====================================================================
%%  Helper functions %%====================================================================
