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

channel_list_test_() ->
    ChannelName = "bosshack123",
    Channels = [#channel{name=ChannelName}],
    ServerState = #serverstate{channels=Channels},
    Result = server:handle_call({channel_list}, self(), ServerState),
    ?_assertEqual({reply, [ChannelName], ServerState}, Result).

join_test_() ->
    {ok, ServerPid} = server:start_link(),
    Username = "tester",
    ChannelName = "bosshack",
    SelfPid = self(),
    server:connect(ServerPid, Username),
    {setup,
     fun() ->
        meck:new(channel),
        meck:expect(channel, start_link, 0, {ok, SelfPid}),
        meck:expect(channel, join, 2, ok),
        server:join(ServerPid, ChannelName)
     end,
     fun(_) ->
        meck:unload(channel)
     end,
     [?_assertEqual([ChannelName], server:channel_list(ServerPid)),
      ?_assert(meck:called(channel, start_link, [])),
      ?_assert(meck:called(channel, join, [SelfPid, meck:is(fun(U) -> U#user.username == Username end)]))
     ]

    }.

send_test_() ->
    Username = "tester",
    ChannelName = "bosshack",
    Channel = #channel{name=ChannelName,pid=self()},
    User = #user{username=Username,pid=self()},
    MessageText = "Hello World",
    ServerStateWithUserAndChannel = #serverstate{channels=[Channel],listeners=[User]},
    {setup,
     fun() ->
        meck:new(channel),
        meck:expect(channel, join, 2, ok),
        meck:expect(channel, send, 3, ok)
     end,
     fun(_) ->
        meck:unload(channel)
     end,
     [
      ?_assertMatch({noreply, ServerStateWithUserAndChannel},
            server:handle_cast({send, self(), ChannelName, MessageText}, ServerStateWithUserAndChannel)),
      ?_assert(meck:called(channel, send, [Channel#channel.pid, self(), MessageText]))
     ]

    }.
    

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

assert_received(Inbound) ->
    Message = receive_message(),
    [?_assertEqual(Inbound, Message)].

receive_message() ->
    receive
        M -> M
    after ?TIMEOUT ->
        throw("Message expected.")
    end.

-endif.