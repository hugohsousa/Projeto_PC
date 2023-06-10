-module(loginManager).
-export([create_account/4,close_account/4, login/4, logout/4]).

create_account(User, Pass, Map, From) ->
    case maps:find(User, Map) of
        error ->
            if
               Pass == ""  ->
                    From ! {invalid_password, Map};
               true ->
                    From ! {done_acc, Map#{User => {Pass, false, 0, 0}}}
            end;
        _ ->
            From ! {user_exists, Map}
   end.

close_account(User, Pass, Map, From) ->
    case maps:find(User, Map) of
        {ok, {Pass, _, _, _}} ->
            From ! {done_remove, maps:remove(User, Map)};
        error ->
            From ! {invalid_user, Map};
        _ ->
            From ! {invalid_password, Map}
    end.

login(User, Pass, Map, From) ->
    case maps:find(User, Map) of
        {ok, {Pass, false, _, _}} ->
            From ! {done_login, Map#{User := {Pass, true, level, score}}};
        {ok, {Pass, true, _, _}} ->
            From ! {already_login, Map};
        error ->
            From ! {invalid_user, Map};
        _ ->
            From ! {invalid_password, Map}
        end.

logout(User, Pass, Map, From) ->
    case maps:find(User, Map) of
        {ok, {Pass, true, _, _}} ->
            From ! {done_logout, Map#{User := {Pass, false}}};
        {ok, {Pass, false, _, _}} ->
            From ! {already_logout, Map};
        error ->
            From ! {invalid_user, Map};
        _ ->
            From ! {invalid_password, Map}
        end.
