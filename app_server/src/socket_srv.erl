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
            gen_tcp:send(Socket, list_to_binary("Say hello from server!")),
            main_loop(State);
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