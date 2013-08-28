%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author Josh Adams <josh@isotope11.com> %%%
%%% @copyright (c) 2013, Josh Adams         %%%
%%% @doc                                    %%%
%%% @end                                    %%%
%%%                                         %%%
%%% Created 2013-08-17 by Josh Adams        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(chatserver).
-behaviour(gen_server).

%%% API
-export([start_link/0, join/2, nicklist/1, part/1, send/2]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
                 terminate/2, code_change/3]).

-include_lib("chatserver_records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%%%%%%%%%%%%%%%%
%%% Client API %%%
%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, #state{}, []).

join(Pid, Username) ->
    gen_server:call(Pid, {join, Username}).

nicklist(Pid) ->
    gen_server:call(Pid, nicklist).

part(Pid) ->
    gen_server:call(Pid, part).

send(Pid, MessageText) ->
    gen_server:cast(Pid, {send, self(), MessageText}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(State) -> {ok, State}. %% no treatment of info here!

handle_call({join, Username}, From, #state{}=State) ->
    case lists:any(fun(X) -> X#user.username == Username end, State#state.listeners) of
        true -> {reply, duplicate_username, State};
        false -> NewState = add_user(Username, From, State),
            broadcast_join_message(Username, NewState),
            {reply, ok, NewState}
    end;
handle_call(nicklist, _From, #state{}=State) ->
    {reply, list_usernames(State), State};
handle_call(part, From, #state{}=State) ->
    {FromPid, _} = From,
    User = user_from_pid(FromPid, State),
    Username = User#user.username,
    broadcast_part_message(Username, State),
    NewState = remove_user(From, State),
    {reply, ok, NewState}.
%handle_call({send, MessageText}, From, #state{}=State) ->
    %broadcast(MessageText, From, State),
    %{reply, { ok, MessageText }, State}.

handle_cast({send, FromPid, MessageText}, #state{}=State) ->
    broadcast(MessageText, FromPid, State),
    {noreply, State};
handle_cast(_, #state{}=State) ->
    io:format("No idea", State).

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    io:format("terminating..."),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% Code changing is not presently supported.  This
    %% function exists to satisfy the behaviour.
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
list_usernames(State) ->
    MapToUsername = fun(X) ->
        X#user.username
    end,
    lists:map(MapToUsername, State#state.listeners).

add_user(Username, {FromPid, _Ref}, State) ->
    NewUser = #user{username=Username, pid=FromPid},
    State#state{listeners=[NewUser|State#state.listeners]}.

remove_user({FromPid, _Ref}, State) ->
    OldListeners = State#state.listeners,
    NewListeners = lists:filter(fun(User) -> User#user.pid =/= FromPid end, OldListeners),
    State#state{listeners=NewListeners}.

broadcast(MessageText, FromPid, State) ->
    Message = build_message(MessageText, FromPid, State),
    broadcast(Message, State).
broadcast(Message, #state{}=State) ->
    lists:foreach(fun(L) -> broadcast(Message, L#user.pid) end, State#state.listeners);
broadcast(Message, ToPid) ->
    ToPid ! Message.

build_message(MessageText, FromPid, State) ->
    User = user_from_pid(FromPid, State),
    Username = User#user.username,
    #message{username=Username, text=MessageText}.
build_message(MessageText) ->
    #message{username="system", text=MessageText}.

user_from_pid(FromPid, State) ->
    hd(lists:filter(fun(User) -> User#user.pid == FromPid end, State#state.listeners)).

broadcast_join_message(Username, State) ->
    Message = build_message(Username ++ " has joined."),
    broadcast(Message, State).

broadcast_part_message(Username, State) ->
    Message = build_message(Username ++ " has parted."),
    broadcast(Message, State).

