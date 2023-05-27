-module(server).
-export([start/1, stop/0]).

% Starts server on Port and registers its pid on ?MODULE macro
start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

% Sends stop message to server pid
stop() -> ?MODULE ! stop.


% {reuseaddr, true|false}, defaults to false, if true the local binding addr {IP,Port} of the socket can be reused immediately. No waiting in state CLOSE_WAIT is performed (can be required for high-throughput servers).

% gen_tcp:listen(Port, Options) -> {ok, ListenSocket} | {error, Reason} // options: list, binary, 

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr,true}]),
    Match = spawn(fun() -> match([]) end),
    spawn(fun() -> acceptor(LSock, Match) end),
    receive stop -> ok end.

% get_tcp:accept(Port) -> {ok, Socket} | {error, Reason} // accepts an incoming connection request on a listening socket. Socket must be a socket returned from listen/2

acceptor(LSock, Match) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Match) end),
    Match ! {enter, self()},
    player(Sock, Match).

match(Pids) ->
    receive
        {enter, Pid} ->
            io:format("player entered ~p~n", [Pid]),
            match([Pid | Pids]);
        {line, Data} = Msg ->
            io:format("received ~p~n", [Data]),
            [Pid ! Msg || Pid <- Pids],
            match(Pids);
        {leave, Pid} ->
            io:format("player left ~p~n", [Pid]),
            match(Pids -- [Pid])
    end.

player(Sock, Match) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, Data),
            player(Sock, Match);
        {tcp, _, Data} ->
            Match ! {line, Data},
            player(Sock, Match);
        {tcp_closed, _} ->
            Match ! {leave, self()};
        {tcp_error, _, _} ->
            Match ! {leave, self()}
    end.
