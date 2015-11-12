-module(app_server_log).

-export([
    start_link/0, 
    write/3
]).

-export([
    log_proc_init/0
]).

-include("app_server.hrl").

-define(LOG_PROC, ?MODULE).


write (Level, Message, Arguments) ->
	case get(the_player_id) of
		undefined -> ?LOG_PROC ! {log, {Level, Message, Arguments}};
		PlayerId  -> ?LOG_PROC ! {log, {Level, PlayerId, Message, Arguments}}
	end.
    
start_link () ->
    proc_lib:start_link(?MODULE, log_proc_init, []).
 
log_proc_init () ->
    register(?LOG_PROC, self()),
    proc_lib:init_ack({ok, self()}),
	{Time, _} = erlang:localtime(),
	{ok, File} = get_log_file(Time),
    log_proc_loop(Time, File).

log_proc_loop (Time, File) ->
    receive
        {log, Log} ->
			{NowTime, _} = erlang:localtime(),
			case NowTime of
				Time ->
					case catch write_log(Log, File) of
						{'EXIT', Reason} ->
							io:format("write log failed:~p~n", [Reason]),
							log_proc_loop(Time, File);
						_ ->
							log_proc_loop(Time, File)
					end;
				_ ->
					ok = file:close(File),
					{ok, NewFile} = get_log_file(NowTime),
					case catch write_log(Log, NewFile) of
						{'EXIT', Reason} ->
							io:format("write log failed:~p~n", [Reason]),
							log_proc_loop(NowTime, NewFile);
						_ ->
							log_proc_loop(NowTime, NewFile)
					end
			end;
			
		_ ->
			log_proc_loop(Time, File)
    end.
	
write_log ({info, PlayerId, Message, Arguments}, _) ->
    io:format("[info] from player " ++ integer_to_list(PlayerId) ++ " : " ++ Message, Arguments);
	
write_log ({Level, PlayerId, Message, Arguments}, File) ->
    LogTitle = get_log_title(PlayerId),
    LogContent = io_lib:format(LogTitle ++ Message ++ "~n~n", Arguments),
    LogBin = list_to_binary(LogContent),
    ok = file:write(File, LogBin),
	if ?IS_DEBUG ->
		{{Y, M, D}, {HH, MM, SS}} = erlang:localtime(),
		io:format(
			"~n~p-~p-~p ~p:~p:~p [~p] from player ~p~n" ++ Message ++ "~n~n", 
			[Y, M, D, HH, MM, SS, Level, PlayerId] ++ Arguments
		);
	true ->
		ok
	end;
	
write_log ({info, Message, Arguments}, _) ->
    io:format("[info] : " ++ Message, Arguments);
	
write_log ({Level, Message, Arguments}, File) ->
    LogTitle = get_log_title(),
    LogContent = io_lib:format(LogTitle ++ Message ++ "~n~n", Arguments),
    LogBin = list_to_binary(LogContent),
    ok = file:write(File, LogBin),
	if ?IS_DEBUG ->
		{{Y, M, D}, {HH, MM, SS}} = erlang:localtime(),
		io:format(
			"~n~p-~p-~p ~p:~p:~p [~p]~n" ++ Message ++ "~n~n", 
			[Y, M, D, HH, MM, SS, Level] ++ Arguments
		);
	true ->
		ok
	end.
    
get_log_file({Y, M, D}) ->
    FileName = "log/" ++ integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D) ++ "/error.log",
    case filelib:is_file(FileName) of
        true -> ok;
        false -> ok = filelib:ensure_dir(FileName)
    end,
	file:open(FileName, [append, raw, {delayed_write, 1024 * 100, 2000}]).
    
get_log_title(PlayerId) ->
    {{Y, M, D}, {HH, MM, SS}} = erlang:localtime(),
    integer_to_list(Y) ++ "-" ++ 
    integer_to_list(M) ++ "-" ++ 
    integer_to_list(D) ++ " " ++ 
    integer_to_list(HH) ++ ":" ++ 
    integer_to_list(MM) ++ ":" ++ 
    integer_to_list(SS) ++ " from player " ++
	integer_to_list(PlayerId) ++ "~n".
	
get_log_title() ->
    {{Y, M, D}, {HH, MM, SS}} = erlang:localtime(),
    integer_to_list(Y) ++ "-" ++ 
    integer_to_list(M) ++ "-" ++ 
    integer_to_list(D) ++ " " ++ 
    integer_to_list(HH) ++ ":" ++ 
    integer_to_list(MM) ++ ":" ++ 
    integer_to_list(SS) ++ "~n".