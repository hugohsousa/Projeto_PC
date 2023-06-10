-module(gameLogic).
-export([findCollisions/1,calcCollision/6,norm/1]).
-define(AngularVel, 1.5).
-define(LinearAcel, 1).

findCollisions(State) ->
    {P1, P2, Food} = State,
    {TmpP1, TmpP2} = findCollisionsPlayers(P1, P2),
    {NewP1, NewP2, ToRemove} = findCollisionsPlayerFood(TmpP1, TmpP2, Food, []),
    NewFood = cleanFood(Food, ToRemove),
    {NewP1, NewP2, NewFood}.

findCollisionsPlayers(P1,P2) ->
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
                   NewP1 = P1,
                   NewP2 = P2
           end;
        _ ->
            NewP1 = P1,
            NewP2 = P2
    end, {NewP1,NewP2}.

findCollisionsPlayerFood(P1, P2, [], ToRemove) -> {P1, P2, ToRemove};
findCollisionsPlayerFood(P1, P2 ,[H|T], ToRemove) ->
    {US1, XP1, YP1, C1, RP1, DP1, AVP1, LVP1, SP1, FromPid1} = P1,
    {US2, XP2, YP2, C2, RP2, DP2, AVP2, LVP2, SP2, FromPid2} = P2,
    {Color, FoodX, FoodY} = H, 
    case calcCollision(XP1, YP1, RP1, FoodX, FoodY, 10) of
        false ->
            case calcCollision(XP2, YP2, RP2, FoodX, FoodY, 10) of
                true ->
                    case Color of
                        "red" ->
                            NewP2 = {US2, XP2, YP2, C2, RP2, DP2, ?AngularVel, ?LinearAcel, SP2, FromPid2};
                        "green" ->
                            if AVP2 < ?AngularVel * 5 ->
                                   NewP2 = {US2, XP2, YP2, C2, RP2, DP2, AVP2 + 1, LVP2, SP2, FromPid2};
                               true ->
                                   NewP2 = {US2, XP2, YP2, C2, RP2, DP2, AVP2, LVP2, SP2, FromPid2}
                            end;
                        "blue" ->
                            if LVP2 < ?LinearAcel * 5 ->
                                   NewP2 = {US2, XP2, YP2, C2, RP2, DP2, AVP2, LVP2 + 1, SP2, FromPid2};
                               true ->
                                   NewP2 = {US2, XP2, YP2, C2, RP2, DP2, AVP2, LVP2, SP2, FromPid2}
                            end
                    end,
                    {NewP1, NewP2, NewToRemove} = findCollisionsPlayerFood(P1, P2, T, ToRemove ++ [H]);
                false ->
                    {NewP1, NewP2, NewToRemove} = findCollisionsPlayerFood(P1, P2, T, ToRemove)
            end;
        true ->
            case Color of
                "red" ->
                    NewP1 = {US1, XP1, YP1, C1, RP1, DP1, AVP1, LVP1, SP1, FromPid1};
                "green" ->
                    if AVP1 < ?AngularVel * 5 ->
                           NewP1 = {US1, XP1, YP1, C1, RP1, DP1, AVP1 + 1, LVP1, SP1, FromPid1};
                       true ->
                           NewP1 = {US1, XP1, YP1, C1, RP1, DP1, AVP1, LVP1, SP1, FromPid1}
                    end;
                "blue" ->
                    if LVP1 < ?LinearAcel * 5 ->
                           NewP1 = {US1, XP1, YP1, C1, RP1, DP1, AVP1, LVP1 + 1, SP1, FromPid1};
                       true ->
                           NewP1 = {US1, XP1, YP1, C1, RP1, DP1, AVP1, LVP1, SP1, FromPid1}
                    end
            end,
            {NewP1, NewP2, NewToRemove} = findCollisionsPlayerFood(P1, P2, T, ToRemove ++ [H])
    end,
    {NewP1, NewP2, NewToRemove}.
    
cleanFood(F, []) -> F;
cleanFood([H|T],[H2|T2]) ->
    {C1, X1, Y1} = H, 
    {C2, X2, Y2} = H2, 
    case {C2, X2, Y2} of
        {C1, X1, Y1} ->
            NewFood = cleanFood(T,T2);
        _ ->
            NewFood = [H] ++ cleanFood(T,[H2|T2])
    end, 
    NewFood.

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
    if ((Angle1 =< 3.14/2) + (Angle2 > 3.14/2)) ->
           Res = player1;
        true ->
            if ((Angle2 =< 3.14/2) + (Angle1 > 3.14/2)) ->
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

