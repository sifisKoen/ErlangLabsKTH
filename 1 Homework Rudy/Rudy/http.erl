-module(http).

-export([parse_request/1, ok/1, get/1]).

parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

% Request section

% 32 is the space in ASCII
request_line([$G, $E, $T, 32 | R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13, 10 | R3] = R2,
    %Return this tuple
    %The first element is the parsed representation of the request line
    %And the R3 is the rest of the string
    {{get, URI, Ver}, R3}.

% URI for request

request_uri([32 | R0]) ->
    {[], R0};
request_uri([C | R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C | Rest], R1}.

%Version Information for request section

%We represent the versions with atoms v10 and v11
%We can switch on the atoms than again parsing a string
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

% Headers section

%First function consumes a sequence of headers
headers([13, 10 | R0]) ->
    {[], R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header | Rest], R2}.
%Second function consumes individual headers
header([13, 10 | R0]) ->
    {[], R0};
header([C | R0]) ->
    {Rest, R1} = header(R0),
    {[C | Rest], R1}.

% The body section

message_body(R) ->
    {R, []}.

ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.
get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
