-module(app_server_app).

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

-include("app_server_app.hrl").


start () ->
    application:start(?MODULE).
    
start (_Type, _StartArgs) ->
    file:make_dir(?LOG_DIR),
    inets:start(),
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    
    start_game_app_log(),
    start_socket_proxy_sup(),
    start_socket_sup(),
    
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