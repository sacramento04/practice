-module(client_socket_srv).

-export([
    start_link/1,
    init/1
]).

-export([
    kill_for_stop/1
]).

-define(TIME_OUT, infinity).

-include("app_client.hrl").


start_link (Id) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Id])}.
    
init (Id) ->
    Socket = wait_connect(Id),

    case catch main_loop(#client_state{id = Id, socket = Socket}) of
        {'EXIT', R} ->
            ?ERROR("[main_loop] exit, reason: ~p~n", [R]);
        _ ->
            ok
    end.
    
wait_connect (Id) ->
    Options = [
        binary, 
        {active, false}, 
        {packet, 4}
    ],
    
    case gen_tcp:connect(?SERVER_IP, ?SERVER_PORT, Options, ?TIME_OUT) of
        {ok, Socket} ->
            Socket;
        Reason ->
            ?INFO("[wait_connect] connect failed, reason: ~p~n", [Reason]),
            wait_connect(Id)
    end.
    
main_loop (State) ->
    Id = State #client_state.id,
    Socket = State #client_state.socket,
    inet:setopts(Socket, [{active, once}]),
    
    receive
        {tcp, Socket, Answer} ->
            ?INFO("[main_loop] socket [~p] receive data: ~p~n", [Id, Answer]),
            main_loop(State);
        {tcp_closed, Socket} ->
            ?INFO("[main_loop] socket [~p] closed~n", [Id]),
            clean(State);
        {tcp_error, Socket, Reason} ->
            ?INFO("[main_loop] socket [~p] error, reason: ~p~n", [Id, Reason]),
            clean(State);
        {send_data, Bin} ->
            gen_tcp:send(Socket, Bin),
            main_loop(State);
        kill_for_stop ->
            ?INFO("[main_loop] process closed by stop~n", []),
            clean(State);
        Msg ->
            ?INFO("[main_loop] process receive message: ~p~n", [Msg]),
            clean(State)
    end.
    
clean (State) ->
    gen_tcp:close(State #client_state.socket).
    
kill_for_stop (Pid) ->
    Pid ! kill_for_stop.