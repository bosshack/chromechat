# ChromeChat

A chat server written in erlang.  It consists of the following components:

- chatserver
    - Responds to the following messages:
        - { join, { Pid, username } }
        - { send, { Pid, message } }
        - { part, Pid }
    - Supports the following records:
        - { state { listeners=[], messages=[] } }
        - { message, { username, message } }
        - { user, { username, pid } }
