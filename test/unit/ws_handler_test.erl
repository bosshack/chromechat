-module(ws_handler_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(TIMEOUT, 50).

%%%%%%%%%%%%%
%%% Tests %%%
%%%%%%%%%%%%%

init_test_() ->
    meck:new(server),
    meck:expect(server, connect, 2, {ok, self()}),
    meck:expect(server, join, 2, {ok, self()}),
    {ok, Thing, _} = ws_handler:websocket_init(1, 2, 3),
    ?_assertEqual(Thing, 2).

%%%%%%%%%%%%%%%%%%%%%%%
%%% Setup Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    self().

stop(_) ->
    ok.

-endif.
