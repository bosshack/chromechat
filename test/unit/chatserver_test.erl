%% This is my first attempt at unit testing a server in erlang.
%% Was pointed to this as an example: https://github.com/blt/locker/blob/master/src/lk_proc.erl#L144
-module(chatserver_test).
-include_lib("eunit/include/eunit.hrl").
-include("../../src/chatserver_records.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Descriptions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

join_test_() ->
    {"A client can connect to the server.",
     ?setup(fun can_connect/1)}.

nicklist_test_() ->
    {"A client can list the connected users.",
     ?setup(fun can_list_users/1)}.

send_test_() ->
    {"A client can send messages.",
     ?setup(fun can_send_message/1)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% Setup Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    self().

stop(_) ->
    "no-op.".

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Nothing to see here yet...

%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests %%%
%%%%%%%%%%%%%%%%%%%%
can_connect(Self) ->
    InitialState = #state{listeners=[], messages=[]},
    [?_assertMatch({reply, ok}, chatserver:handle_call({join, "knewter"}, Self, InitialState))].

can_list_users(Self) ->
    Knewter = #user{username="knewter", pid=Self},
    StateWithUser = #state{listeners=[Knewter], messages=[]},
    [?_assertMatch({reply, ["knewter"]}, chatserver:handle_call(nicklist, Self, StateWithUser))].

can_send_message(Self) ->
    Knewter = #user{username="knewter", pid=Self},
    StateWithUser = #state{listeners=[Knewter], messages=[]},
    [?_assertMatch({reply, { ok, "some message" }}, chatserver:handle_call({send, "some message" }, Self, StateWithUser))].
