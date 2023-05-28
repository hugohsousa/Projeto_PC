-module(loginManager).
-export([create_account/4]).

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
