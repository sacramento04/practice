-ifdef(debug).
    -define(IS_DEBUG, true).
    -define(DEBUG(Msg, Args), io:format("[debug] - " ++ Msg, Args)).
    -define(TRACE(Call), io:format("[trace][~p,~p]~n    code: ~s ~n    return: ~w~n", [?MODULE, ?LINE, ??Call, Call])).
-else.
    -define(IS_DEBUG, false).
    -define(DEBUG(Msg, Args), ok).
    -define(TRACE(Call), ok).
-endif.

-define(INFO(Msg, Args), app_client_log:write(info, Msg, Args)).
-define(ERROR(Msg, Args), app_client_log:write(error, Msg, Args)).
-define(WARNING(Msg, Args), app_client_log:write(warning, Msg, Args)).

-define(LOG_DIR, "./log/").

-define(SERVER_IP, "192.168.24.88").
-define(SERVER_PORT, 8889).

-record(
    client_state, 
    {
        id = 0,
        player_id = undefined,
        socket = undefined
    }
).