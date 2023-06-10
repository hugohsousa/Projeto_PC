-module(server).
-export([start/1, stop/0]).
-import(loginManager, [create_account/4,close_account/4,login/4,logout/4]).
-import(game, [match/2, clientMatchLoop/3]). 
-import(gameLogic, [findCollisions/1]).

% Starts server on Port and registers its pid on ??MODULE macro
start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

% Sends stop message to server pid
stop() -> ?MODULE ! stop.


% {reuseaddr, true|false}, defaults to false, if true the local binding addr {IP,Port} of the socket can be reused immediately. No waiting in state CLOSE_WAIT is performed (can be required for high-throughput servers).

% gen_tcp:listen(Port, Options) -> {ok, ListenSocket} | {error, Reason} // options: list, binary, 

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr,true}]),
    Lobby = spawn(fun() -> lobby([]) end),
    spawn(fun() -> acceptor(LSock, Lobby) end),
    loop(Lobby, #{}, 0),
    receive stop -> ok end.

loop(Lobby, Users, Games) ->
    receive
        {login, User, Pwd, FromPid} ->
            io:format("Debug gameLoop login received ~p~p~p~n", [User, Pwd, FromPid]),
            {Res, UpdatedUsers} = loginManager:login(User, Pwd, Users, FromPid),
            FromPid ! Res,
            loop(Lobby, UpdatedUsers, Games);
        {logout, User, Pwd, FromPid} ->
            io:format("Debug gameLoop logout received ~p~p~p~n", [User, Pwd, FromPid]),
            {Res, UpdatedUsers} = loginManager:logout(User, Pwd, Users, FromPid),
            FromPid ! Res,
            loop(Lobby, UpdatedUsers, Games);
        {create_account, User, Pwd, FromPid} ->
            io:format("Debug gameLoop create_account received ~p~p~p~n", [User, Pwd, FromPid]),
            {Res, UpdatedUsers} = loginManager:create_account(User, Pwd, Users, FromPid),
            FromPid ! Res,
            loop(Lobby, UpdatedUsers, Games);
        {remove_account, User, Pwd, FromPid} ->
            io:format("Debug gameLoop remove_account received ~p~p~p~n", [User, Pwd, FromPid]),
            {Res, UpdatedUsers} = loginManager:close_account(User, Pwd, Users, FromPid),
            FromPid ! Res,
            loop(Lobby, UpdatedUsers, Games);
        {join, User, FromPid} ->
            Lobby ! {joinLobby, User, FromPid},
            loop(Lobby, Users, Games);
        {start, _FromPid} ->
            NewLobby = spawn(fun() -> lobby([]) end),
            loop(NewLobby, Users, Games + 1)
    end.

% get_tcp:accept(Port) -> {ok, Socket} | {error, Reason} // accepts an incoming connection request on a listening socket. Socket must be a socket returned from listen/2

acceptor(LSock, Lobby) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Lobby) end),
    % Lobby ! {enter, self()},
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
            io:format("Debug case login, message: ~p~n", [Message]),
            [User, Pwd] = string:split(Message, "#"),
            ?MODULE ! {login, User, Pwd, self()},
            receive 
                done -> gen_tcp:send(Sock, "login:done\n");
                invalid_user -> gen_tcp:send(Sock, "login:invalid_user\n");
                invalid_password -> gen_tcp:send(Sock, "login:invalid_password\n");
                already_login -> gen_tcp:send(Sock, "login:already_login\n")
            end;
        ["logout", Message] ->
            io:format("Debug case logout, message: ~p~n", [Message]),
            [User, Pwd] = string:split(Message, "#"),
            ?MODULE ! {logout, User, Pwd, self()},
            receive 
                done -> gen_tcp:send(Sock, "logout:done\n");
                invalid_user -> gen_tcp:send(Sock, "logout:invalid_user\n");
                invalid_password -> gen_tcp:send(Sock, "logout:invalid_password\n");
                already_logout-> gen_tcp:send(Sock, "logout:already_logout\n")
            end;
        ["create_account", Message] ->
            io:format("Debug case create_account, message: ~p~n", [Message]),
            [User, Pwd] = string:split(Message, "#"),
            ?MODULE ! {create_account, User, Pwd, self()},
            receive 
                done -> gen_tcp:send(Sock, "create_account:done\n");
                user_exists -> gen_tcp:send(Sock, "create_account:user_exists\n");
                invalid_password -> gen_tcp:send(Sock, "create_account:invalid_password\n")
            end;
        ["remove_account", Message] ->
            io:format("Debug case remove_account, message: ~p~n", [Message]),
            [User, Pwd] = string:split(Message, "#"),
            ?MODULE ! {remove_account, User, Pwd, self()},
            receive 
                done -> gen_tcp:send(Sock, "remove_account:done\n");
                invalid_user -> gen_tcp:send(Sock, "remove_account:invalid_user\n");
                invalid_password -> gen_tcp:send(Sock, "remove_account:invalid_password\n")
            end;
        ["join", Username] ->
            io:format("Debug case join,  username: ~p~n", [Username]),
            ?MODULE ! {join, Username, self()},
            receive
                {done, MatchPid} ->
                    gen_tcp:send(Sock, "Done\n"),
                    clientMatchLoop(Sock, MatchPid, Username)
            end;
       [_, Message] ->
            io:format("Debug input not recognized, message: ~p~n", [Message])
    end.

lobby(Queue) ->
    receive 
        {joinLobby, User, FromPid} ->
            NewQueue = Queue ++ [{User, FromPid}],
            case length(NewQueue) of
                2 ->
                    match(?MODULE, NewQueue); %spawn?
                _ ->
                    lobby(NewQueue)
            end
    end.
%        {enter, Pid} ->
%            io:format("player entered ~p~p~n", [self(), Pid]),
%            lobby([Pid | Pids]);
%        {line, Data} = Msg ->
%            io:format("received ~p~p~n", [self(), Data]),
%            [Pid ! Msg || Pid <- Pids],
%            lobby(Pids);
%        {leave, Pid} ->
%            io:format("player left ~p~p~n", [self(), Pid]),
%            lobby(Pids -- [Pid]);
%        {error, Pid} ->
%            io:format("tcp_error received"),
%            lobby(Pids -- [Pid])

