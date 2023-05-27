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
    Lobby = spawn(fun() -> lobby([]) end),
    spawn(fun() -> acceptor(LSock, Lobby) end),
    receive stop -> ok end.

% get_tcp:accept(Port) -> {ok, Socket} | {error, Reason} // accepts an incoming connection request on a listening socket. Socket must be a socket returned from listen/2

acceptor(LSock, Lobby) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Lobby) end),
    Lobby ! {enter, self()},
    client(Sock, Lobby).

client(Sock, Lobby) ->
    receive
        {tcp_closed, _} ->
            Lobby ! {leave, self()};
        {tcp_error, _, _} ->
            Lobby ! {error, self()};
        {tcp, _, Data} ->
            % Lobby ! {line, Data},
            parseClientInput(Data, Sock),
            client(Sock, Lobby)
    end.

parseClientInput(Data, Sock) ->
    String = binary_to_list(string:trim(Data, trailing, "\n")),
    case string:split(String, ":") of
        ["login", Message] ->
            io:format("case login, message: ~p~n", [Message]);
        ["logout", Message] ->
            io:format("case logout, message: ~p~n", [Message]);
        ["create_account", Message] ->
            io:format("case create_account, message: ~p~n", [Message]);
        ["remove_account", Message] ->
            io:format("case remove_account, message: ~p~n", [Message]);
        [_, Message] ->
            io:format("input no recognized, message: ~p~n", [Message])
    end.

lobby(Pids) ->
    receive
        {enter, Pid} ->
            io:format("player entered ~p~p~n", [self(), Pid]),
            lobby([Pid | Pids]);
        {line, Data} = Msg ->
            io:format("received ~p~p~n", [self(), Data]),
            [Pid ! Msg || Pid <- Pids],
            lobby(Pids);
        {leave, Pid} ->
            io:format("player left ~p~p~n", [self(), Pid]),
            lobby(Pids -- [Pid]);
        {error, Pid} ->
            io:format("tcp_error received"),
            lobby(Pids -- [Pid])
    end.

