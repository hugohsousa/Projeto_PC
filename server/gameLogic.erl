-module(gameLogic).
-export([findColisions/1,calcCollision/6,norm/1]).

findColisions(State) ->
    {P1, P2, Food} = State,
    {NewP1, NewP2} = findColisionsPlayers(P1,P2),
    %{P1, Food} = findColisionsPlayerFood(P1, Food),
    %{P2, Food} = findColisionsPlayerFood(P2, Food),
    {NewP1,NewP2,Food}.


findColisionsPlayers(P1,P2) ->
    {US1, XP1, YP1, C1, RP1, DP1, AVP1, LVP1, SP1, FromPid1} = P1,
    {US2, XP2, YP2, C2, RP2, DP2, AVP2, LVP2, SP2, FromPid2} = P2,
    case calcCollision(XP1, YP1, RP1, XP2, YP2, RP2) of 
        true ->
           case calcCollisionAngle(XP1, YP1,DP1, XP2, YP2, DP2) of
               player1 ->
                   NewP1 = {US1, XP1, YP1, C1, RP1, DP1, AVP1, LVP1, SP1 + 1, FromPid1},
                   NewP2 = {US2, 700, 700, C2, RP2, DP2, AVP2, LVP2, SP2, FromPid2};
               player2 ->
                   NewP1 = {US1, 100, 100, C1, RP1, DP1, AVP1, LVP1, SP1, FromPid1},
                   NewP2 = {US2, XP2, YP2, C2, RP2, DP2, AVP2, LVP2, SP2 + 1, FromPid2};
               _ ->
                   NewP1 = "teste",
                   NewP2 = P2
           end;
        _ ->
            NewP1 = P1,
            NewP2 = P2
    end, {NewP1,NewP2}.

findColisionsPlayerFood(P, Food) ->
    ok.

calcCollision(XP1, YP1, RaioP1, XP2, YP2, RaioP2) ->
    Dist = math:sqrt((XP2-XP1)*(XP2-XP1) + (YP2-YP1)*(YP2-YP1)), 
    if Dist < RaioP1 + RaioP2 -> 
           Res = true;
        true -> 
           Res = false
    end, 
    Res.
    
calcCollisionAngle(XP1, YP1, DP1, XP2, YP2, DP2) ->
    VisionVectorP1 = {math:cos(DP1), math:sin(DP1)},
    VisionVectorP2 = {math:cos(DP2), math:sin(DP2)},
    Vector1 = {XP2-XP1,YP2-YP1},
    Vector2 = {XP1-XP2,YP1-YP2},
    Angle1 = math:acos(crossProduct(Vector1,VisionVectorP2)/(norm(Vector1)*norm(VisionVectorP2))),
    Angle2 = math:acos(crossProduct(Vector1,VisionVectorP1)/(norm(Vector2)*norm(VisionVectorP1))),
    io:format("Debug Case Angle1: ~p Angle2: ~p~n",[Angle1,Angle2]),
    if ((Angle1 =< 90) + (Angle2 > 90)) ->
           Res = player1;
        true ->
            if ((Angle2 =< 90) + (Angle1 > 90)) ->
                Res = player2;
            true ->
                Res = nothing
            end
    end, 
    Res.

norm(Vector) ->
    {X, Y} = Vector,
    math:sqrt((X*X)+(Y*Y)).

crossProduct(Vector1, Vector2) ->
    {X1 , Y1} = Vector1,
    {X2 , Y2} = Vector2,
    ((X1*X2) + (Y1*Y2)).

