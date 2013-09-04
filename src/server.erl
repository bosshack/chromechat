-module(server).
-behavior(gen_server).

%%% API
-export([start_link/0, connect/2, join/2, disconnect/1, part/2, send/3, channel_list/1, list_users/1]).

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

channel_list(ServerPid) ->
    gen_server:call(ServerPid, {channel_list}).

join(ServerPid, ChannelName) ->
    gen_server:call(ServerPid, {join, ChannelName}).

part(ServerPid, ChannelName) ->
    gen_server:cast(ServerPid, {part, self(), ChannelName}).

send(ServerPid, ChannelName, MessageText) ->
    gen_server:cast(ServerPid, {send, self(), ChannelName, MessageText}).

list_users(ServerPid) ->
    gen_server:call(ServerPid, {list_users}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(ServerState) -> {ok, ServerState}.

handle_call({connect, Username}, From, #serverstate{}=ServerState) ->
    {Status, NewState} = add_user(Username, From, ServerState),
    {reply, Status, NewState};
handle_call({disconnect}, From, #serverstate{}=ServerState) ->
    NewState = remove_user(From, ServerState),
    {reply, ok, NewState};
handle_call({channel_list}, _From, #serverstate{}=ServerState) ->
    GetChannelName = fun(X) -> X#channel.name end,
    ChannelNames = lists:map(GetChannelName, ServerState#serverstate.channels),
    {reply, ChannelNames, ServerState};
handle_call({join, ChannelName}, From, #serverstate{}=ServerState) ->
    {FromPid, _} = From,
    case user_from_pid(FromPid, ServerState) of
        not_found -> {reply, user_not_found, ServerState};
        User -> case channel_from_name(ChannelName, ServerState) of
            not_found -> {ok, ChannelPid} = channel:start_link(),
                NewServerState = #serverstate{listeners=ServerState#serverstate.listeners,
                    messages=ServerState#serverstate.messages, 
                    channels=[#channel{name=ChannelName,pid=ChannelPid}|ServerState#serverstate.channels]},
                {reply, channel:join(ChannelPid, User), NewServerState};
            Channel -> {reply, channel:join(Channel#channel.pid, User), ServerState}
        end
    end;
handle_call({list_users}, _From, #serverstate{}=ServerState) ->
    {reply, ServerState#serverstate.listeners, ServerState}.

handle_cast({send, FromPid, ChannelName, MessageText}, #serverstate{}=ServerState) ->
    Channel = channel_from_name(ChannelName, ServerState),
    case Channel of
        not_found -> io:format("Channel not found " ++ ChannelName);
        _ -> channel:send(Channel#channel.pid, FromPid, MessageText)

    end,
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

remove_user({FromPid, _Ref}, ServerState) ->
    OldListeners = ServerState#serverstate.listeners,
    NewListeners = lists:filter(fun(User) -> User#user.pid =/= FromPid end, OldListeners),
    ServerState#serverstate{channels=ServerState#serverstate.channels, listeners=NewListeners}.

user_from_pid(FromPid, ServerState) ->
    case lists:filter(fun(User) -> User#user.pid == FromPid end, ServerState#serverstate.listeners) of
        [] -> not_found;
        [X | _] -> X
    end.

channel_from_name(Name, ServerState) ->
    case lists:filter(fun(Channel) -> Channel#channel.name == Name end, ServerState#serverstate.channels) of
        [] -> not_found;
        [X | _] -> X
    end.
