-module(test).

-export([bench/3]).

% We need to pass the number of loops of witch the server will send messages.
% test:bench(100, localhost, 8080).
bench(NumberOfLoops, Host, Port) ->
    % server:start(Port, )
    Start = erlang:system_time(micro_seconds),
    run(NumberOfLoops, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    Finish - Start.

run(NumberOfLoops, Host, Port) ->
    if
        NumberOfLoops == 0 ->
            ok;
        true ->
            request(Host, Port),
            run(NumberOfLoops - 1, Host, Port)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    % First connects to the server / port
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    % Second sends a message to the server
    % gen_tcp:send(Socket, Message)
    % Socket = Server, Message = http:get("foo")
    %           ( Socket,Packet)
    gen_tcp:send(Server, http:get("foo")),
    % Third receive something from the server
    % Server = Socket
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        % We don't care what is the receive message we just need to know if we have ok in our receive message
        % {ok, Packet} ->
        {ok, _} ->
            % io:format(Packet ++ "~n"),
            ok;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end,
    % Last the request will close the connection with the Server (Socket)
    gen_tcp:close(Server).
