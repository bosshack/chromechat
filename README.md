# ChromeChat
[![Build Status](https://travis-ci.org/bosshack/chromechat.png?branch=master)](https://travis-ci.org/bosshack/chromechat)

A chat server written in erlang.  It consists of the following components:

- chatserver
    - Responds to the following messages:
        - { connect, { ServerPid, username } }
            - This will reject a connection if your username is already taken.
            - otherwise, the user is added to the 'list of connected users' in some fashion.
        - { disconnect, { ServerPid } }
        - { channel_list, { ServerPid } }
        - { send, { ServerPid, ChannelName, MessageText } }
            - Will not let you send to a channel that you have not joined.
        - { join, { ServerPid, channelname } }
            - Only allows a user to join the same channel once.
            - If you're the first user to join this channel, create the corresponding channel.
            - Proxy the join message on behalf of the sender to this channel (which is its own running OTP server)
         - { part, { ServerPid, channelname } }
            - Removes the user from the channel with that name.
            - If not in the channel or no channel by that name, 'succeed.'
- channel
    - Responds to the following messages:
        - { join, { Pid, username } }
        - { send, { Pid, message } }
        - { nicklist, Pid }
        - { part, Pid }
    - Supports the following records:
        - { state { listeners=[], messages=[] } }
        - { message, { username, text } }
        - { user, { username, pid } }

## Development

To run the tests:

```
make test
```

To run the server:

First, make the project:

```
make
```

Then, run it.

```
bin/chromechat start
```

Now you can go to http://localhost:8000

To run the server as a daemonized process:
```
bin/chromechat start -detached
```

To stop the server:
```
bin/chromechat stop
```

NOTE: Presently the server hosts a channel, rather than a chatserver, due to an
inadequate original design.  We're working on building out the actual chatserver
component at present :)

## Basic in-console tests

Just a few notes on how you can use this as I get it built, since I'm new to erlang and forget this stuff:

### Playing with the channel directly.

```erlang
c(channel, [debug_info]).
rr(channel).
{ok, Pid} = channel:start_link().
channel:join(Pid, "knewter").
channel:nicklist(Pid).
channel:part(Pid).
channel:nicklist(Pid).
channel:join(Pid, "knewter").
channel:send(Pid, "Message test").
```

### Trying out communication between two processes via the server

This is a fun little thing using erlang's distribution.  Try the following:

Run the first shell with `erl -sname foo`

Then run another shell in another terminal window with `erl -sname bar`

In the `foo` shell, run the following:

```erlang
net_kernel:connect_node(bar@jibbajabba). % This is jibbajabba for me, because it's my machine name
nodes(). % you should see [bar@jibbajabba]
```

Then, in the `bar` shell, verify that there's a connection by doing the following:

```erlang
nodes(). % you should see [foo@jibbajabba]
```

Congratulations, now you've connected two erlang VMs.  To make them communicate, we will give the shell processes registered names in each VM:

```erlang
% in foo
register(shell, self()).
```

Then do the same for bar.

```erlang
% in bar
register(shell, self()).
```

Now you can verify they can communicate by just sending a message from foo to bar:

```erlang
% in foo
{shell, bar@jibbajabba} ! "Hey".
```

And read the message from bar:

```erlang
% in bar
flush(). % You should see the message was received
```

Good, they're talking to each other.  Now, let's run a server in foo, then connect to it as one user in foo, and connect to it as another user in bar. We'll have both join the `boss` channel. Finally, let's have a user send a message to the chat server, and verify the other user had the message broadcast to them.

```erlang
% in foo
c(server).
rr(server).
c(channel).
rr(channel).
{ok, ServerPid} = server:start_link().
server:connect(ServerPid, "foo_user").
server:join(ServerPid, "boss"). % Join the boss channel
```

Then join from bar:

```erlang
% in bar
c(server).
rr(server).
c(channel).
rr(channel).
% uh oh, we need the pid...let's prepare to receive it:
receive
  ServerPid -> ok
end.
```

So at this point, we need the pid of the already-started channel so that we can join it.  Bar's already waiting to be given the pid, so let's just send it from foo now:

```erlang
% in foo
{shell, bar@jibbajabba} ! Pid.
```

Now we can verify that bar now knows this Pid:

```erlang
% in bar
ServerPid. % You should see a pid that begins with something other than 0 be printed out
```

Now that bar knows the Pid to talk to the channel, let's have him join:

```erlang
% in bar
server:connect(ServerPid, "bar_user").
server:join(ServerPid, "boss").
```

That's it, we have two distinct erlang VMs talking to the channel now.  Let's finish up by sending a message via the chat server:

```erlang
% in foo
server:send(ServerPid, "boss", "Hey guys!").
flush(). % Since we're connecting, we should have had our own message broadcast back to us.
```

Now verify that bar got the message:

```erlang
% in bar
flush(). % Should see a hey guys message from foo_user
```

That's a successful chat server, if presently kind of unwieldy! :D

## Deployment

Currently following deployment from https://github.com/6/heroku-erlang-example/

