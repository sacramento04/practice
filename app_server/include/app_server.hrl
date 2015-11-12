-ifdef(debug).
    -define(IS_DEBUG, true).
    -define(DEBUG(Msg, Args), io:format("[debug] - " ++ Msg, Args)).
    -define(TRACE(Call), io:format("[trace][~p,~p]~n    code: ~s ~n    return: ~w~n", [?MODULE, ?LINE, ??Call, Call])).
-else.
    -define(IS_DEBUG, false).
    -define(DEBUG(Msg, Args), ok).
    -define(TRACE(Call), ok).
-endif.

-define(INFO(Msg, Args), app_server_log:write(info, Msg, Args)).
-define(ERROR(Msg, Args), app_server_log:write(error, Msg, Args)).
-define(WARNING(Msg, Args), app_server_log:write(warning, Msg, Args)).

-define(GET_ENV(Key, Default), (
    case application:get_env(Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end
)).
-define(GET_ENV_STR(Key, Default), (
    case application:get_env(Key) of
        {ok, Val} -> if 
            is_atom(Val)    -> atom_to_list(Val); 
            is_integer(Val) -> integer_to_list(Val);
            true -> Val 
        end;
        undefined -> Default
    end
)).
-define(GET_ENV_INT(Key, Default), (
    case application:get_env(Key) of
        {ok, Val} -> if 
            is_list(Val) -> list_to_integer(Val);
            is_atom(Val) -> list_to_integer(atom_to_list(Val)); 
            true -> Val 
        end;
        undefined -> Default
    end
)).
-define(GET_ENV_ATOM(Key, Default), (
    case application:get_env(Key) of
        {ok, Val} -> if 
            is_list(Val)    -> list_to_atom(Val);
            is_integer(Val) -> integer_to_list(list_to_atom(Val)); 
            true -> Val 
        end;
        undefined -> Default
    end
)).

-define(LOG_DIR, "./log/").

-record(
    client_state, 
    {
        player_id = undefined,
        socket = undefined
    }
).