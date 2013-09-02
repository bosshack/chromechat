-module(server).
-behavior(gen_server).

%%% API
-export([start_link/0, connect/2, disconnect/1, part/2, send/3]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
                 terminate/2, code_change/3]).

-include("./channel_records.hrl").

%%%%%%%%%%%%%%%%%%
%%% Client API %%%
%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, #serverstate{}, []).

connect(ServerPid, Username) ->
    gen_server:call(ServerPid, {connect, Username}).

disconnect(ServerPid) ->
    gen_server:call(ServerPid, {disconnect}).

part(ServerPid, ChannelName) ->
    gen_server:cast(ServerPid, {part, self(), ChannelName}).

send(ServerPid, ChannelName, MessageText) ->
    gen_server:cast(ServerPid, {send, self(), MessageText}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(ServerState) -> {ok, ServerState}.

handle_call({connect, Username}, From, #serverstate{}=ServerState) ->
    {Status, NewState} = add_user(Username, From, ServerState),
    {reply, Status, NewState}.

handle_cast({send, FromPid, MessageText}, #serverstate{}=ServerState) ->
    {noreply, ServerState}.

handle_info(Msg, ServerState) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, ServerState}.

terminate(normal, _ServerState) ->
    io:format("terminating..."),
    ok.

code_change(_OldVsn, ServerState, _Extra) ->
    %% Code changing is not presently supported.  This
    %% function exists to satisfy the behaviour.
    {ok, ServerState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

add_user(Username, {FromPid, _Ref}, ServerState) ->
    case lists:any(fun(X) -> X#user.username == Username end, ServerState#serverstate.listeners) of
        true -> {duplicate_username, ServerState};
        false -> NewUser = #user{username=Username, pid=FromPid},
                 {ok, ServerState#serverstate{listeners=[NewUser|ServerState#serverstate.listeners]}}
    end.

user_from_pid(FromPid, ServerState) ->
    hd(lists:filter(fun(User) -> User#user.pid == FromPid end, ServerState#serverstate.listeners)).
