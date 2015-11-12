-module(app_ets).

-export([
    init/0
]).

-include("app_server.hrl").
-include("gen/game_db.hrl").


init () ->
    ets:new(player_username_index, [ordered_set, named_table, public, {keypos, 1}]),
    create_player_index(ets:first(game_db:ets(player))).
    
create_player_index('$end_of_table') ->
	ok;
create_player_index(Key) ->
	[Row] = ets:lookup(game_db:ets(player), Key),
	ets:insert(player_username_index, {Row #player.username, Row #player.id}),
	create_player_index(ets:next(game_db:ets(player), Key)).