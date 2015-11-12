-module(app_client).

-behavious(application).
-behavious(supervisor).

-export([
    start/0,
    start/2,
    stop/1
]).

-export([
    init/1
]).

-include("app_client.hrl").


start () ->
    application:start(?MODULE).
    
start (_Type, _StartArgs) ->
    file:make_dir(?LOG_DIR),
    inets:start(),
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    
    start_app_client_log(),
    start_client_socket_sup(),
    start_reloader(),
    
    Result.
    
stop (_State) ->
    ok.
    
init ([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.
    
start_app_client_log () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            app_client_log, 
            {app_client_log, start_link, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [app_client_log]
        }
    ).
    
start_client_socket_sup () ->
    {ok, _} = supervisor:start_child(
        ?MODULE,
        {
            client_socket_sup,
            {client_socket_sup, start_link, []},
            permanent,
            16#FFFFFFFF,
            supervisor,
            [client_socket_sup]
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