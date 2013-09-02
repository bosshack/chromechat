-module(server_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/channel_records.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(TIMEOUT, 50).

%%%%%%%%%%%%%
%%% Tests %%%
%%%%%%%%%%%%%

connect_test_() ->
    Username = "tester",
    {ok, ServerPid} = server:start_link(),
    ?_assertEqual(ok, server:connect(ServerPid, Username)).

disconnect_test_() ->
    Username = "tester",
    {ok, ServerPid} = server:start_link(),
    server:connect(ServerPid, Username),
    ?_assertEqual(ok, server:disconnect(ServerPid)).

%%%%%%%%%%%%%%%%%%%%%%%
%%% Setup Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    self().

stop(_) ->
    flush(),
    ok.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.

-endif.