-module(gameLogic).
-export([calcCollision/3,inside/6,norm/2]).

calcCollision(P1, P2, Pid) ->
    {XP1, YP1, C1, RP1} = P1,
    {XP2, YP2, C2, RP2} = P2,
    case inside(XP1, YP1, RP1, XP2, YP2, RP2) of 
        true ->
           Pid ! collision;
        false ->
           Pid ! no_collision 
    end.


inside(XP1, YP1, RaioP1, XP2, YP2, RaioP2) ->
    Dist = math:sqrt((XP2-XP1)*(XP2-XP1) + (YP2-YP1)*(YP2-YP1)), 
    if Dist < RaioP1 + RaioP2 -> 
           Res = true;
        true -> 
           Res = false
    end, Res.
    

norm(X, Y) -> 
    math:sqrt((X*X)+(Y*Y)).
