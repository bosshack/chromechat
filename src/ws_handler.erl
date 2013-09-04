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
    channel:join(channel_server, "knewter"),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    channel:send(channel_server, Msg),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Msg, Req, State) ->
    User = Msg#message.username,
    Text = Msg#message.text,
    MessagePrinted = User ++ ": " ++ Text,
    {reply, {text, MessagePrinted}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
