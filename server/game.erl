-module(game).
-export([match/2, initMatch/1]).

match(ServerPid, Players) ->
    io:format("Starting match\n"),
    ServerPid ! {start, self()},    
    [FromPid ! {start, self()} || {_User, FromPid} <- Players],
    State = initMatch(Players),
    matchTimer(State, []).

initMatch(Players) ->
    [{User1, FromPid1},{User2, FromPid2}] = Players,
    P1 = [User1, "black", 100, 100, 5.5, 0.23, 2.13, 0, FromPid1],
    P2 = [User2, "white", 700, 700, 2.36, 0.23, 2.13, 0, FromPid2],
    State = [P1, P2],
    State.

matchTimer(State, Food) ->
    Self = self(),
    spawn(fun() -> receive after 40 -> Self ! timeout end end),
    {NewState, TmpFood} = handleMatch(State, Food),
    % new food = genFood
    % NewState is a list so already parsed? ///  PlayerInfo = parseMatch(maps:to_list(NewState), []),
    % [FromPid ! Info || {asdas d} <- maps:to_list(NewState)],
    loop(NewState, TmpFood). % sub by NewFood

handleMatch(State, Food) ->
    FinalState = findCollisions(State),
    FinalState.

findCollisions(State) -> 
    % State = [ [User1, "black", 100, 100, 5.5, 0.23, 2.13, 0, FromPid1],  [User2, "white", 700, 700, 2.36, 0.23, 2.13, 0, FromPid2] ]
    {XP1, YP1} = { lists:nth(3, lists:nth(1, State)), lists:nth(4, lists:nth(1, State)) }, 
    {XP2, YP2} = { lists:nth(3, lists:nth(2, State)), lists:nth(4, lists:nth(2, State)) },
    Raio = 50,
    Dist = math:sqrt((XP2-XP1)*(XP2-XP1) + (YP2-YP1)*(YP2-YP1)), 
    if Dist > 2 * Raio ->  % mf just crashed
           ok;  % figure out who rear ended who 
       true ->
           ok
    end.
    %FinalState leva nova pos de quem foi atropelado
    %FinalState.

loop(Players, Food) ->
    receive
        timeout ->
            matchTimer(Players, Food);
        {leave, User, FromPid} ->
            FromPid ! leave
            % removePlayer
    end.

input() ->
    ok.

% ProcessingPlayer = {username, colr, x, y, direction}
% ServerPlayer = {username, color, x, y, direction, linearAcel, angularVeloc, score}
% state = {}
