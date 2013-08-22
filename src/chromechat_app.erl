-module(chromechat_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/", toppage_handler, []},
                    {"/websocket", ws_handler, []}
                ]}
        ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], % so this will start an http server on port 8080, and the routes are up there ^ ^ ^
        [{env, [{dispatch, Dispatch}]}]),
    chromechat_sup:start_link().

stop(_State) ->
    ok.
