# ChromeChat
[![Build Status](https://travis-ci.org/bosshack/chromechat.png?branch=master)](https://travis-ci.org/bosshack/chromechat)

A chat server written in erlang.  It consists of the following components:

- chatserver
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
./start.sh
```

Now you can go to http://localhost:8080

## Basic in-console tests

Just a few notes on how you can use this as I get it built, since I'm new to erlang and forget this stuff:

### Playing with the chatserver directly.

```erlang
c(chatserver, [debug_info]).
rr(chatserver).
{ok, Pid} = chatserver:start_link().
chatserver:join(Pid, "knewter").
chatserver:nicklist(Pid).
chatserver:part(Pid).
chatserver:nicklist(Pid).
chatserver:join(Pid, "knewter").
chatserver:send(Pid, "Message test").
```

### Trying out communication between two processes via the chatserver

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

Good, they're talking to each other.  Now, let's run a chatserver in foo, then connect to it as one user in foo, and connect to it as another user in bar.  Finally, let's have a user send a message to the chat server, and verify the other user had the message broadcast to them.

```erlang
% in foo
c(chatserver).
rr(chatserver).
{ok, Pid} = chatserver:start_link().
chatserver:join(Pid, "foo_user").
```

Then join from bar:

```erlang
% in bar
c(chatserver).
rr(chatserver).
% uh oh, we need the pid...let's prepare to receive it:
receive
  ServerPid -> ok
end.
```

So at this point, we need the pid of the already-started chatserver so that we can join it.  Bar's already waiting to be given the pid, so let's just send it from foo now:

```erlang
% in foo
{shell, bar@jibbajabba} ! Pid.
```

Now we can verify that bar now knows this Pid:

```erlang
% in bar
ServerPid. % You should see a pid that begins with something other than 0 be printed out
```

Now that bar knows the Pid to talk to the chatserver, let's have him join:

```erlang
% in bar
chatserver:join(ServerPid, "bar_user").
% Now list the users
chatserver:nicklist(ServerPid). % Should see two - ["foo_user", "bar_user"]
```

That's it, we have two distinct erlang VMs talking to the chatserver now.  Let's finish up by sending a message via the chat server:

```erlang
% in foo
chatserver:send(Pid, "Hey guys!").
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

