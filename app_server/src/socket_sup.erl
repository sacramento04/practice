-module(socket_sup).

-behavious(supervisor).

-export([
    start_link/0,
    init/1
]).

-export([
    start_child/0,
    count_child/0,
    kill_all/0
]).

-include("app_server.hrl").


start_link () ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
init ([]) ->
    {
        ok,
        {
            {simple_one_for_one, 10, 10},
            [
                {
                    socket_srv,
                    {socket_srv, start_link, []},
                    temporary,
                    brutal_kill,
                    worker,
                    [socket_srv]
                }
            ]
        }
    }.
    
start_child () ->
    supervisor:start_child(?MODULE, []).
    
count_child () ->
    case lists:keyfind(workers, 1, supervisor:count_children(?MODULE)) of
        {workers, Amount} ->
            Amount;
        _ ->
            0
    end.
    
kill_all () ->
    [{links, Pids}] = process_info(whereis(?MODULE), [links]),
    AppPid = whereis(app_server),
    
    lists:foreach(
        fun(Pid) ->
            if
                Pid =/= AppPid ->
                    socket_srv:kill_for_stop(Pid);
                true ->
                    ok
            end
        end,
        Pids
    ).