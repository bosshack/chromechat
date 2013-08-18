# ChromeChat

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

## Basic in-console tests
Just a few notes on how you can use this as I get it built, since I'm new to erlang and forget this stuff:

### Playing with the chatserver directly.

```
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
