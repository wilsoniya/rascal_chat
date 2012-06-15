#!/usr/bin/env escript

start_server() ->
    Opts = [{packet, 0}, {reuseaddr, true}, {active, true}, {ip, {0,0,0,0}}],
    {ok, Listen} = gen_tcp:listen(20000, Opts),
    init_ets(),
    spawn(fun() -> connection_server(Listen) end),
    sleep(infinity).

init_ets() ->
    ets:new(user_socks, [set, named_table, public]).

sleep(Duration) ->
    receive
    after Duration ->
        true
    end.

connection_server(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> connection_server(Listen) end),
    inet:setopts(Socket, [{packet, 0}, {nodelay, true}, {active, true}]),
    client(Socket).

client(Socket) ->
    receive
        {tcp, Socket, Msg} ->
            [Op|Args] = string:tokens(Msg, ";"),
            case Op of 
                "print" ->
                    io:format("~p~n", Args),
                    client(Socket);
                "register" ->
                    [Username|_] = Args,
                    case register_user(Username) of 
                        username_exists ->
                            io:format("Username exists: ~s~n", [Username]),
                            gen_tcp:send(Socket, "username_exists");
                        ok ->
                            put(username, Username),
                            io:format("Username registered: ~s~n", [Username]),
                            gen_tcp:send(Socket, "ok")
                    end,
                    client(Socket);
                "deregister" ->
                    deregister_user(get(username)),
                    client(Socket);
                "send" ->
                    [Message|_] = Args,
                    io:format("got message: ~s~n", [Message]),
                    Username = get(username),
                    spawn(fun() -> send_message(Message, Username) end),
                    client(Socket);
                Other ->
                    io:format("Unknown op; exiting~n"),
                    io:format("op: ~p~n", [Op]),
                    io:format("message body: ~p~n", [Msg])
            end;
        {tcp_closed, Socket} ->
            Username = get(username),
            erase(username),
            ets:delete(user_socks, Username),
            io:format("~s has gone offline~n", [Username]);
        {inbound_msg, Msg, Sender} ->
            gen_tcp:send(Socket, string:join([Sender, Msg], ";")),
            client(Socket)
    end.

deregister_user(Username) ->
    ets:delete(user_socks, Username).

register_user(Username) ->
    ClientPid = self(),
    ExistingUsernames = ets:lookup(user_socks, Username),
    if
        length(ExistingUsernames) > 0 ->
            username_exists;
        true ->
            ets:insert(user_socks, {Username, ClientPid}),
            ok
    end.  

send_message(Msg, Sender) ->
    Send = fun({Username, ClientPid}, _) ->
        if 
            Sender /= Username ->
                ClientPid ! {inbound_msg, Msg, Sender},
                ok;
            true ->
                ok
        end,
        none
    end,
    ets:foldl(Send, none, user_socks),  
    ok.

main(_) ->
    start_server().

