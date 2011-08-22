%% http://jerith.za.net/writings/erlangsockettut.html

-module(multiecho).
-export([listen/1]).

%% TCP options for our listening socket.  The initial list atom
%% specifies that we should receive data as lists of bytes (ie
%% strings) rather than binary objects and the rest are explained
%% better in the Erlang docs than I can do here.

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).

%% Listen on the given port, accept the first incoming connection and
%% launch the echo loop on it.

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    do_accept(LSocket).

%% The accept gets its own function so we can loop easily.  Yay tail
%% recursion!

do_accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> do_echo(Socket) end),
    do_accept(LSocket).

%% Sit in a loop, echoing everything that comes in on the socket.
%% Exits cleanly on client disconnect.

do_echo(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            do_echo(Socket);
        {error, closed} ->
            ok
    end.

