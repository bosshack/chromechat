-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include_lib("channel_records.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    %% Connect to our chat server on behalf of this connected user...
    %% FIXME: This needs to be something you can set - ideally with a message to the chatserver (/nick)
    server:connect(chatserver, "knewter"),
    server:join(chatserver, "boss"),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    server:send(chatserver, "boss", Msg),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Msg, Req, State) ->
    MessageJson = json_from_message(Msg),
    {reply, {text, MessageJson}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

json_from_message(Msg) ->
    User = Msg#message.username,
    Text = Msg#message.text,
    MessageStructure = message_structure_for(User, Text),
    jiffy:encode(MessageStructure).

message_structure_for(User, Text) when is_binary(User) and is_binary(Text) ->
    {[{type, message}, {username, User}, {text, Text}]};
message_structure_for(User, Text) when is_binary(User) ->
    message_structure_for(User, list_to_binary(Text));
message_structure_for(User, Text) when is_binary(Text) ->
    message_structure_for(list_to_binary(User), Text);
message_structure_for(User, Text) ->
    message_structure_for(list_to_binary(User), list_to_binary(Text)).
