-module(loginManager).
-export([create_account/4,close_account/4, login/4]).

create_account(User, Pass, Map, From) ->
    case maps:find(User, Map) of
        error ->
            if
               Pass == ""  ->
                    From ! {invalid_password, Map};
               true ->
                    From ! {done, Map#{User => {Pass, false}}}
            end;
        _ ->
            From ! {user_exists, Map}
   end.

close_account(User, Pass, Map, From) ->
    case maps:find(User, Map) of
        {ok, {Pass, _}} ->
            From ! {done, maps:remove(User, Map)};
        error ->
            From ! {invalid_user, Map};
        _ ->
            From ! {invalid_password, Map}
    end.

login(User, Pass, Map, From) ->
    case maps:find(User, Map) of
        {ok, {Pass, false}} ->
            From ! {done, maps:update(User, {Pass,true}, Map)};
        error ->
            From ! {invalid_user, Map};
        _ ->
            From ! {invalid_password, Map}
        end.
