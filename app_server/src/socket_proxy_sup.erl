-module(socket_proxy_sup).

-behavious(supervisor).

-export([
    start_link/0,
    init/1
]).

-export([
    acceptor_start/2,
    acceptor_wait/2
]).

-define(SOCKET_ACCEPTOR, 2).
-define(LISTEN_PORT, 8889).

-include("app_server.hrl").


start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
init ([]) ->
    ListenSocket = listen(),
    
    ChildSpecs = lists:foldl(
        fun(I, Specs) ->
            Id = list_to_atom("listen_sock_" ++ integer_to_list(I)),
            
            Spec = {
                Id,
                {?MODULE, acceptor_start, [Id, ListenSocket]},
                transient,
                16#FFFFFFFF,
                worker,
                [?MODULE]
            },
            
            [Spec | Specs]
        end,
        [],
        lists:seq(1, ?SOCKET_ACCEPTOR)
    ),
    
    {ok, {{one_for_one, 1, 10}, ChildSpecs}}.
    
listen () ->
    Options = [
        binary,
        {packet, 4},
        {packet_size, 1024},
        {reuseaddr, true},
        {backlog, 5},
        {active, true}
    ],
    
    case gen_tcp:listen(?LISTEN_PORT, Options) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            throw({error, {tcp_listen, Reason}})
    end.
    
acceptor_start (Id, LSocket) ->
    {ok, proc_lib:spawn_link(?MODULE, acceptor_wait, [Id, LSocket])}.
    
acceptor_wait (Id, LSocket) ->
    register(Id, self()),
    acceptor_loop(LSocket).
    
acceptor_loop (LSocket) ->
    case (catch gen_tcp:accept(LSocket, 10000)) of
        {ok, ASocket} ->
            {ok, {Address, Port}} = inet:peername(ASocket),
            ?INFO("Connect From ~p:~p~n", [Address, Port]),
            try_handle_connection(ASocket),
            acceptor_loop(LSocket);
        {error, Reason} ->
            handle_error(Reason),
            acceptor_loop(LSocket);
        {'EXIT', Reason} ->
            handle_error({'EXIT', Reason}),
            acceptor_loop(LSocket)
    end.
    
try_handle_connection (Socket) ->
    case socket_sup:start_child() of
        {ok, Pid} ->
            socket_srv:set_socket(Pid, Socket);
        {error, Reason} ->
            gen_tcp:close(Socket),
            handle_error(Reason)
    end.
    
handle_error (timeout) ->
    ok;

handle_error ({enfile, _}) ->
    sleep(200);

handle_error (emfile) ->
    sleep(200);
    
handle_error (max_conn) ->
    sleep(200);

handle_error (closed) ->
    exit(normal);

handle_error (econnreset) ->
    exit(normal);

handle_error (econnaborted) ->
    ok;
	
handle_error ({handle_connection, Reason}) ->
	?ERROR("handle_connection: ~p~n", [Reason]);

handle_error ({'EXIT', Reason}) ->
    String = lists:flatten(io_lib:format("Accept exit: ~p", [Reason])),
    accept_failed(String);

handle_error (Reason) ->
    String = lists:flatten(io_lib:format("Accept error: ~p", [Reason])),
    accept_failed(String).

accept_failed (String) ->
    ?ERROR(String, []),
    exit({accept_failed, String}).    

sleep (T) -> 
    receive after T -> ok end.