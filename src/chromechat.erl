-module(chromechat).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

start() ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(sasl),
    ensure_started(cowboy),
    {ok, ChannelPid} = channel:start_link(),
    register(channel_server, ChannelPid),
    application:start(chromechat).

stop() ->
    application:stop(chromechat).
