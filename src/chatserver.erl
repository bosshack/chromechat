-module(chatserver).
-behaviour(gen_server).
-compile(export_all).
-include("chatserver_records.hrl").

handle_call({join, Username}, From, #state{}=S) ->
    ok;
handle_call(nicklist, From, #state{}=S) ->
    MapToUsername = fun(X) ->
        X#user.username
    end,
    lists:map(MapToUsername, S#state.listeners).
