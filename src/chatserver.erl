-module(chatserver).
-behaviour(gen_server).
-export([start_link/0, join/2]).
-export([init/1, handle_call/3, handle_info/2,
                 terminate/2, code_change/3]).
-include("chatserver_records.hrl").

%%% Client API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

join(Pid, Username) ->
    gen_server:call(Pid, {join, Username}).

%%% Server functions
init([]) -> {ok, []}. %% no treatment of info here!

handle_call({join, Username}, From, #state{}=State) ->
    {reply, ok, State};
handle_call(nicklist, From, #state{}=State) ->
    {reply, list_usernames(State), State};
handle_call({send, Message}, From, #state{}=State) ->
    {reply, { ok, Message }, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, State) ->
    io:format("terminating..."),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

%%% Implementation functions
list_usernames(State) ->
    MapToUsername = fun(X) ->
        X#user.username
    end,
    lists:map(MapToUsername, State#state.listeners).
