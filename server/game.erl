-module(game).
-export([match/2, initMatch/1, clientMatchLoop/3]).

match(ServerPid, Players) ->
    io:format("Starting match\n"),
    ServerPid ! {start, self()},    
    [FromPid ! {start, self()} || {_User, FromPid} <- Players],
    State = initMatch(Players),
    matchTimer(State).

initMatch(Players) ->
    % State = [ [Username1, x1, y1, color1,  radius, direction1, angularVel1, linearAcel1, score1, FromPid1 ],
    %           [Username2, x2, y2, color2, radius, direction2, angularVel2, linearAcel2, score2, FromPid2 ],
    %           [[Color, x, y], [Color, x, y]]]
    [{User1, FromPid1},{User2, FromPid2}] = Players,
    P1 = {User1, 100, 100, "black", 20, 5.5, 2.13, 0.23, 0, FromPid1},
    P2 = {User2, 700, 700, "white", 20, 2.36, 2.13, 0.23, 0, FromPid2},
    State = {P1, P2, []},
    State.

matchTimer(State) ->
    % io:format("Match Timer ~n"),
    Self = self(),
    spawn(fun() -> receive after 1 -> Self ! timeout end end),

    NewState = handleMatch(State),
    % new food = genFood
    % NewState is a list so already parsed? ///  PlayerInfo = parseMatch(maps:to_list(NewState), []),
    % [FromPid ! Info || {asdas d} <- maps:to_list(NewState)],
    loop(NewState). % sub by NewFood

handleMatch(State) ->
    FinalState = findCollisions(State),
    FinalState.

findCollisions(State) -> 
    State.

loop(State) ->
    receive
        timeout ->
            matchTimer(State);
        {leave, User, FromPid} ->
            FromPid ! leave;
            % removePlayer
        {move, {Username, Direction} ,_} ->
            NewState = input(State, Username, Direction),
            loop(NewState)
    end.

clientMatchLoop(Sock, LobbyPid, Username) ->
    receive
        {tcp, _, Data} -> 
            String = binary_to_list(string:trim(Data, trailing, "\n")),
            case string:split(String, ":") of
                ["move", Direction] ->
                    io:format("Debug case move in clientMatchLoop: ~p~p~n", [Direction, Username]),
                    gen_tcp:send(Sock, "arrived clientMatchLoop \n"),
                    LobbyPid ! {move, {Username, Direction}, self()}
            end,
            clientMatchLoop(Sock, LobbyPid, Username)
    end.

input(State, Username, Direction) ->
    {{User1, _, _, _, _, _, _, _, _, _}, {User2, _, _, _, _, _, _, _, _, _}, _} = State,
    {P1, P2, Food} = State,
    case Username of
        User1 ->
            {User, X, Y, Color, Radius, Dir, AngVel, LinAcel, Score, FromPid} = P1;
        User2 ->
            {User, X, Y, Color, Radius, Dir, AngVel, LinAcel, Score, FromPid} = P2
    end,
    case Direction of
        "left" ->
            NewDir = Dir + AngVel, % Dir + (AngVel / tickrate)? , AVP = m/s  , tickrate = tick/s
            NewX = X,
            NewY = Y;
        "right" ->
            NewDir = Dir - AngVel,
            NewX = X,
            NewY = Y;
        "up" ->
            NewDir = Dir,
            NewX = X + (LinAcel * (math:cos(NewDir) - x)),
            NewY = Y + (LinAcel * (math:sen(NewDir) - y));
        "up#left" ->
            NewDir = Dir + AngVel,
            NewX = X + (LinAcel * (math:cos(NewDir) - x)),
            NewY = Y + (LinAcel * (math:sen(NewDir) - y));
        "up#right" ->    
            NewDir = Dir - AngVel,
            NewX = X + (LinAcel * (math:cos(NewDir) - x)),
            NewY = Y + (LinAcel * (math:sen(NewDir) - y));
        _ ->
            io:format("Debug input error ~p~n", [Dir]),
            NewDir = Dir,
            NewX = X,
            NewY = Y
    end,
    case Username of
        User1 ->
            NewP1 = {User, NewX, NewY, Color, Radius, NewDir, AngVel, LinAcel, Score, FromPid},
            NewState = {NewP1, P2, Food};
        User2 ->
            NewP2 = {User, NewX, NewY, Color, Radius, NewDir, AngVel, LinAcel, Score, FromPid},
            NewState = {P1, NewP2, Food}
    end,
    NewState.

% ProcessingPlayer = {username, colr, x, y, direction}
% ServerPlayer = {username, color, x, y, direction, linearAcel, angularVeloc, score}
% state = {}
