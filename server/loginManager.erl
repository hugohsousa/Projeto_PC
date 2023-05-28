-module(loginManager).
-export([create_account/4,close_account/4]).

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
        {ok, {_, _}} ->
            From ! {invalid_password, Map};
        _ ->
            From ! {invalid_user, Map}
    end.

