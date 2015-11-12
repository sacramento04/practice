-module(client_socket_sup).

-behavious(supervisor).

-export([
    start_link/0,
    init/1
]).

-export([
    start_children/1,
    count_child/0,
    send_data/1,
    kill_all/0  
]).

-include("app_client.hrl").


start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
init ([]) ->
    {
        ok,
        {
            {simple_one_for_one, 10, 10},
            [
                {
                    client_socket_srv,
                    {client_socket_srv, start_link, []},
                    temporary,
                    brutal_kill,
                    worker,
                    [client_socket_srv]
                }
            ]
        }
    }.
    
count_child () ->
    case lists:keyfind(workers, 1, supervisor:count_children(?MODULE)) of
        {workers, Amount} ->
            Amount;
        _ ->
            0
    end.
    
kill_all () ->
    [{links, Pids}] = process_info(whereis(?MODULE), [links]),
    AppPid = whereis(app_client),
    
    lists:foreach(
        fun(Pid) ->
            if
                Pid =/= AppPid ->
                    client_socket_srv:kill_for_stop(Pid);
                true ->
                    ok
            end
        end,
        Pids
    ).
    
start_children (N) ->
    StartId = case get(start_id) of
        undefined -> 1;
        SId -> SId
    end,
    
    lists:foreach(
        fun(Id) ->
            supervisor:start_child(?MODULE, [Id]),
            
            if
                Id < N ->
                    sleep(50);
                true ->
                    ok
            end
        end,
        lists:seq(1, N)
    ), 
    
    put(start_id, StartId + N).
    
send_data (Data) when is_atom(Data) ->
    Bin = atom_to_binary(Data, utf8),
    send_data(Bin);
send_data (Data) when is_list(Data) ->
    Bin = list_to_binary(Data),
    send_data(Bin);
send_data (Data) when is_tuple(Data) ->
    Bin = term_to_binary(Data),
    send_data(Bin);
send_data (Data) when is_binary(Data) ->
    lists:foreach(
		fun({_, Pid, _, _}) ->
			Pid ! {send_data, Data}
		end,
		supervisor:which_children(?MODULE)
	).
    
sleep (T) -> 
    receive after T -> ok end.