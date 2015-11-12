-module(socket_srv).

-export([
    start_link/0,
    init/0
]).

-export([
    set_socket/2,
    kill_for_stop/1
]).

-include("app_server.hrl").


start_link () ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.
    
init () ->
    receive
        {go, Socket} ->
            start_connection(Socket)
    end.
    
start_connection (Socket) ->
    case catch main_loop(#client_state{socket = Socket}) of
        {'EXIT', R} ->
            ?ERROR("[main_loop] exit, reason: ~p~n", [R]);
        _ ->
            ok
    end.
    
main_loop (State) ->
    Socket = State #client_state.socket,
    inet:setopts(Socket, [{active, once}]),
    
    receive
        {tcp, Socket, Request} ->
            ?INFO("[main_loop] socket receive data: ~p~n", [Request]),
            
            case try_route_request(Request, State) of
                error -> 
                    clean(State);
                NewState -> 
                    main_loop(NewState)
            end;
        {tcp_closed, Socket} ->
            ?INFO("[main_loop] socket closed~n", []),
            clean(State);
        {tcp_error, Socket, Reason} ->
            ?INFO("[main_loop] socket error, reason: ~p~n", [Reason]),
            clean(State);
        kill_for_stop ->
            ?INFO("[main_loop] process closed by stop~n", []),
            clean(State);
        Msg ->
            ?INFO("[main_loop] process receive message: ~p~n", [Msg]),
            clean(State)
    end.
    
try_route_request (Request, State) ->
    case catch game_router:route_request(Request, State) of
        NewState when is_record(NewState, client_state) ->
            NewState;
        {'EXIT', {tcp_send_error, Reason}} ->
            handle_request_error(Request, {tcp_send_error, Reason}),
            error;
        {'EXIT', Reason} ->
            case handle_request_error(Request, Reason) of
				ok -> State;
				error -> error
			end;
        Reason when is_atom(Reason) ->
            case handle_request_error(Request, {atom_result, Reason}) of
				ok -> State;
				error -> error
			end;
        Result ->
            case handle_request_error(Request, {unknow_result, Result}) of
				ok -> State;
				error -> error
			end
    end.
    
handle_request_error (Request, Reason) ->
	case get(last_error_time) of
		undefined ->
			{LocalTime, _} = statistics(wall_clock),
			put(last_error_time, LocalTime),
            
			?ERROR(
				"try_route_request: ~n"
				"    Pid     = ~p~n"
				"    Request = ~p~n"
				"    Reason  = ~p~n", 
				[self(), Request, Reason]
			),
			ok;
		LastTime ->
			{LocalTime, _} = statistics(wall_clock),
            
			if 
                LocalTime - LastTime > 1000 ->
                    put(last_error_time, LocalTime),
                    
                    ?ERROR(
                        "try_route_request: ~n"
                        "    Pid     = ~p~n"
                        "    Request = ~p~n"
                        "    Reason  = ~p~n", 
                        [self(), Request, Reason]
                    ),
				
                    ok;
                true ->
                    ?ERROR(
                        "try_route_request: ~n"
                        "    Pid     = ~p~n"
                        "    Request = ~p~n"
                        "    Reason  = ~p~n", 
                        [self(), Request, Reason]
                    ),
				
                    error
			end
	end.
            
clean (State) ->
    gen_tcp:close(State #client_state.socket).
    
set_socket (Pid, Socket) ->
    inet:setopts(
        Socket,
        [
            {active, false},
            {delay_send, true},
            {packet_size, 1024}
        ]
    ),
    
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! {go, Socket}.
    
kill_for_stop (Pid) ->
    Pid ! kill_for_stop.