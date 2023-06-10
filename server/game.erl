-module(game).
-export([match/2, initMatch/1, clientMatchLoop/3, stateToString/1]).
-import(gameLogic, [findCollisions/1]).
-define(ScreenX, 800).
-define(ScreenY, 800).
-define(PlayerRadius, 50).
-define(FoodRadius, 30).

match(ServerPid, Players) ->
    io:format("Starting match\n"),
    ServerPid ! {start, self()},    
    [FromPid ! {done, self()} || {_User, FromPid} <- Players],
    State = initMatch(Players),
    matchTimer(ServerPid, State).

initMatch(Players) ->
    % State = [ [Username1, x1, y1, color1,  radius, direction1, angularVel1, linearAcel1, score1, FromPid1 ],
    %           [Username2, x2, y2, color2, radius, direction2, angularVel2, linearAcel2, score2, FromPid2 ],
    %           [[Color, x, y], [Color, x, y]]]
    [{User1, FromPid1},{User2, FromPid2}] = Players,
    P1 = {User1, 100, 100, "black", ?PlayerRadius, 5.5, 2.13, 0.23, 0, FromPid1},
    P2 = {User2, 700, 700, "white", ?PlayerRadius, 2.36, 2.13, 0.23, 0, FromPid2},
    State = {P1, P2, []},
    State.

matchTimer(ServerPid, State) ->
    Self = self(),
    spawn(fun() -> receive after 150 -> Self ! timeout end end),

    %TmpState = gameLogic:findCollisions(State),
    TmpState = State,
    NewState = createFood(TmpState),
    {{_,_,_,_,_,_,_,_,_,From1},{_,_,_,_,_,_,_,_,_,From2}, _} = State,
    From1 ! {toClient, State},
    From2 ! {toClient, State},
    loop(ServerPid, NewState).

createFood(State) ->
    {P1, P2, Food} = State,
    case rand:uniform(100) of
        1 ->
            case rand:uniform(3) of
                1 ->
                    NewFood = {"green", rand:uniform(?ScreenX - ?FoodRadius) + ?FoodRadius, rand:uniform(?ScreenY - ?FoodRadius) + ?FoodRadius};
                2 ->
                    NewFood = {"blue", rand:uniform(?ScreenX - ?FoodRadius) + ?FoodRadius, rand:uniform(?ScreenY - ?FoodRadius) + ?FoodRadius};
                3 ->
                    NewFood = {"red", rand:uniform(?ScreenX - ?FoodRadius) + ?FoodRadius, rand:uniform(?ScreenY - ?FoodRadius) + ?FoodRadius}
            end,
            NewState = {P1, P2, Food ++ [NewFood]};
        _ ->
            NewState = {P1, P2, Food}
    end,
    NewState.

loop(ServerPid, State) ->
    receive
        timeout ->
            matchTimer(ServerPid, State);
        {leave, _User, FromPid} ->
            FromPid ! leave,
            ServerPid ! left;
            % removePlayer
        {move, {Username, Direction} ,_} ->
            NewState = input(State, Username, Direction),
            loop(ServerPid, NewState)
    end.

clientMatchLoop(Sock, MatchPid, Username) ->
    receive
        {tcp, _, Data} -> 
            String = binary_to_list(string:trim(Data, trailing, "\n")),
            case string:split(String, ":") of
                ["move", Direction] ->
                    io:format("Debug case move in clientMatchLoop: ~p~p~n", [Direction, Username]),
                    gen_tcp:send(Sock, "arrived clientMatchLoop \n"),
                    MatchPid ! {move, {Username, Direction}, self()}
            end,
            clientMatchLoop(Sock, MatchPid, Username);
        {toClient, State} ->
            Str = stateToString(State),
            gen_tcp:send(Sock, Str),
            clientMatchLoop(Sock, MatchPid, Username)
    end.

stateToString(State) ->
    % "game:username, x, y, raio, dir#user2,x,y,raio#cor,x,y#cor,x,y"
    {{Username1,X1,Y1,_,R1,Dir1,_,_,_,_},{Username2,X2,Y2,_,R2,Dir2,_,_,_,_}, Food} = State,
    PlayerStr = lists:flatten(io_lib:format("game:~s,~p,~p,~p,~p#~s,~p,~p,~p,~p", [Username1, X1, Y1, R1, Dir1, 
                                                                                   Username2, X2, Y2, R2, Dir2])),
    FormatFood = fun({Color, X, Y}) -> io_lib:format("#~s,~p,~p", [Color, X, Y]) end,
    FoodStr = lists:map(FormatFood, Food),
    StateToString = string:concat(PlayerStr, FoodStr),
    string:concat(StateToString, "\n").

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
            NewX = X + (LinAcel * (math:cos(NewDir))),
            NewY = Y + (LinAcel * (math:sin(NewDir)));
        "up#left" ->
            NewDir = Dir + AngVel,
            NewX = X + (LinAcel * (math:cos(NewDir))),
            NewY = Y + (LinAcel * (math:sin(NewDir)));
        "up#right" ->    
            NewDir = Dir - AngVel,
            NewX = X + (LinAcel * (math:cos(NewDir))),
            NewY = Y + (LinAcel * (math:sin(NewDir)));
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
