-module(app_server).

-behaviour(application).
-behaviour(supervisor).

-export([
    start/0,
    start/2,
    stop/1
]).

-export([
    init/1
]).

-include("app_server.hrl").


start () ->
    application:start(?MODULE).
    
start (_Type, _StartArgs) ->
    file:make_dir(?LOG_DIR),
    inets:start(),
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    
    start_game_app_log(),
    start_socket_proxy_sup(),
    start_socket_sup(),
    start_gamedb_mysql(),
    
    game_db_init:init(),
    app_ets:init(),
    
    start_gamedb_sync_worker_0(),
    start_gamedb_sync_worker_1(),
    start_gamedb_sync(),
    start_reloader(),
    
    Result.
    
stop (_State) ->
    ok.
    
init ([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.
    
start_game_app_log () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            app_server_log, 
            {app_server_log, start_link, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [app_server_log]
        }
    ).
    
start_socket_proxy_sup () ->
    {ok, _} = supervisor:start_child(
        ?MODULE,
        {
            socket_proxy_sup,
            {socket_proxy_sup, start_link, []},
            permanent,
            16#FFFFFFFF,
            supervisor,
            [socket_proxy_sup]
        }
    ).
    
start_socket_sup () ->
    {ok, _} = supervisor:start_child(
        ?MODULE,
        {
            socket_sup,
            {socket_sup, start_link, []},
            permanent,
            16#FFFFFFFF,
            supervisor,
            [socket_sup]
        }
    ).
    
start_gamedb_mysql () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            gamedb_mysql, 
            {mysql, start_link, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [mysql]
        }
    ).
    
start_gamedb_sync_worker_0 () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            gamedb_sync_worker_0, 
            {game_db_sync, start_worker0, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [game_db_sync]
        }
    ).
    
start_gamedb_sync_worker_1 () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            gamedb_sync_worker_1, 
            {game_db_sync, start_worker1, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [game_db_sync]
        }
    ).
    
start_gamedb_sync () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            gamedb_sync, 
            {game_db_sync, start_proc, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [game_db_sync]
        }
    ).
    
start_reloader () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            reloader, 
            {reloader, start_link, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [reloader]
        }
    ).