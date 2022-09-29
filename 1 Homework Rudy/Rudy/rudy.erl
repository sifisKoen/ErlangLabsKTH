-module(rudy).

-export([init/1]).

init(Port) ->
    % Using this option we can see the bytes as a list of integers instead of a binary structure
    Opt = [list, {active, false}, {reuseaddr, true}],
    % Pass the port number as an argument
    % Sets up a socket to listen on Port on the local host.
    case gen_tcp:listen(Port, Opt) of
        % {ok, ListenSocket}
        {ok, ListenSocket} ->
            % We send the listening socket to Handler
            % Pool rudy
            handler(ListenSocket, 10),
            % Simple  rudy
            % handler(ListenSocket),
            % We delete the close method in this way the server will never close and will be all the time read for new requests
            % gen_tcp:close(ListenSocket),
            ok;
        {error, Error} ->
            io:format("You have an error in Init: ~w~n", [Error])
    end.

% Simple rudy
% handler(ListenSocket) ->
% pool rudy
handler(ListenSocket, NumberOfSockets) ->
    % Accepts an incoming connection request on a listening socket (Listen -> ListeningSocket)
    case gen_tcp:accept(ListenSocket) of
        % {ok, Client} (Client -> Socket)
        % If a connection is established
        {ok, Client} ->
            request(Client);
        % If the ListenSocket (Listen) is closed
        {error, Error} ->
            io:format("You have an error in Handler: ~w~n", [Error])
    end,
    % Handler call it self so the server be up and running
    % This is the method with pool where NumberOfSockets are the processes we cast.
    % Simple rudy
    % handler(ListenSocket).
    % Pool rudy
    spawn(handler(ListenSocket, NumberOfSockets - 1)).

request(Client) ->
    % recv(Socket, Length) -> Socket = Client, Length = 0
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        % {ok, Packet} -> ok, Packet = Str
        {ok, Str} ->
            % io:format(Str),
            Request = http:parse_request(Str),
            % The Request -> {Request, Headers, Body}
            Response = reply(Request),
            % send(Socket, Packet) -> Socket = Client, Response = Packet
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudyProcesses: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    % timer:sleep(40),
    % io:format("URI : "),
    URI ++
        http:ok(
            "<!DOCTYPE html>\n"
            "                <html>\n"
            "                    <body>\n"
            "\n"
            "                        <h1>Hello rudy server</h1>\n"
            "                        <p>!!!!</p>\n"
            "\n"
            "                    </body>\n"
            "                </html>"
        ).
