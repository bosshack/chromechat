%% This is my first attempt at unit testing a server in erlang.
%% Was pointed to this as an example: https://github.com/blt/locker/blob/master/src/lk_proc.erl#L144
-module(channel_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/channel_records.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(TIMEOUT, 50).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Descriptions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

join_and_join_message_is_broadcast_test_() ->
    {"A client joins and is announced to all attached listeners.",
     ?setup(fun is_broadcast_on_join/1)}.

part_and_part_message_is_broadcast_test_() ->
    {"A client parts and it is announced to all attached listeners.",
     ?setup(fun is_broadcast_on_part/1)}.

join_and_appear_in_nicklist_test_() ->
    {"A client sees itself in the nicklist after joining.",
     ?setup(fun can_connect_and_then_see_self_in_nicklist/1)}.

part_and_disappear_from_nicklist_test_() ->
    {"A client that disconnects no longer shows up in the nicklist.",
     ?setup(fun can_disconnect_and_username_is_removed_from_nicklist/1)}.

join_with_duplicate_username_test_() ->
    {ok, Pid} = channel:start_link(),
    Username = "knewter",
    channel:join(Pid, user(self())),
    SecondJoinResult = channel:join(Pid, user(self())),
    [?_assertEqual(SecondJoinResult, duplicate_username),
     ?_assertEqual([Username], channel:nicklist(Pid))].

connect_and_send_message_test_() ->
    {"A client connects and sends a message, and has that message broadcast to it.",
     ?setup(fun can_connect_and_send_message/1)}.

join_test_() ->
    {"A client can connect to the server, which adds its User to the listeners list. [gen_server protocol]",
     ?setup(fun can_connect/1)}.

nicklist_test_() ->
    {"A client can list the connected users. [gen_server protocol]",
     ?setup(fun can_list_users/1)}.

send_test_() ->
    {"A client can send messages. [gen_server protocol]",
        ?setup(fun can_send_message/1)}. % NOTE: Since I don't know how to flush inbound messages, this test must come after the connect_and_send_message_test_

%%%%%%%%%%%%%%%%%%%%%%%
%%% Setup Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    self().

stop(_) ->
    %% Clear out the receive buffer before each test
    %% DICKS THAT'S JUST A SHELL fuNCTION

    flush(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Nothing to see here yet...

%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%
can_connect(SelfPid) ->
    InitialState = #state{listeners=[], messages=[]},
    Knewter = user(SelfPid),
    StateWithUser = #state{listeners=[Knewter], messages=[]},
    [?_assertMatch({reply, ok, StateWithUser}, channel:handle_call({join, user(SelfPid)}, {SelfPid, []}, InitialState))].

can_list_users(SelfPid) ->
    Knewter = user(SelfPid),
    StateWithUser = #state{listeners=[Knewter], messages=[]},
    [?_assertMatch({reply, ["knewter"], StateWithUser}, channel:handle_call(nicklist, {SelfPid, []}, StateWithUser))].

can_send_message(SelfPid) ->
    Knewter = user(SelfPid),
    StateWithUser = #state{listeners=[Knewter], messages=[]},
    [?_assertMatch({noreply, StateWithUser}, channel:handle_cast({send, SelfPid, "some message" }, StateWithUser))].

can_connect_and_then_see_self_in_nicklist(SelfPid) ->
    {ok, Pid} = channel:start_link(),
    channel:join(Pid, user(SelfPid)),
    [?_assertEqual(["knewter"], channel:nicklist(Pid))].

can_disconnect_and_username_is_removed_from_nicklist(SelfPid) ->
    {ok, Pid} = channel:start_link(),
    channel:join(Pid, user(SelfPid)),
    channel:part(Pid),
    [?_assertEqual([], channel:nicklist(Pid))].

can_connect_and_send_message(SelfPid) ->
    {ok, Pid} = channel:start_link(),
    channel:join(Pid, user(SelfPid)),
    flush(),
    channel:send(Pid, "this is a message"),
    assert_received(#message{username="knewter", text="this is a message"}).

is_broadcast_on_join(SelfPid) ->
    {ok, Pid} = channel:start_link(),
    channel:join(Pid, user(SelfPid)),
    assert_received(#message{username="system", text="knewter has joined."}).

is_broadcast_on_part(SelfPid) ->
    {ok, Pid} = channel:start_link(),
    channel:join(Pid, user(SelfPid)),
    flush(),
    channel:part(Pid),
    assert_received(#message{username="system", text="knewter has parted."}).

assert_received(Inbound) ->
    Message = receive_message(),
    [?_assertEqual(Inbound, Message)].

receive_message() ->
    receive
        M -> M
    after ?TIMEOUT ->
        throw("Message expected.")
    end.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.

user(SelfPid) ->
    #user{username="knewter",pid=SelfPid}.

-endif.
