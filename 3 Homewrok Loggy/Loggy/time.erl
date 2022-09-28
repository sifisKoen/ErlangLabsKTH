% Introduce logical time to the worker process.

% It should keep track of its own counter and pass this along with any
% message that is sends to other workers

%  When a worker receive a message must update its timer to the greater of its
% internal clock and the time-stamp of the message before increment its clock

-module(time).

-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% -export([zero/0, update/3]).

zero() -> 0.

inc(Name, Time) ->
    Time + 1.

% Merge the two Lamport time stamps and return the max value
merge(Ti, Tj) ->
    if
        Ti < Tj ->
            Tj;
        true ->
            Ti
    end.

% True if Ti is less than or equal to Tj
leq(Ti, Tj) ->
    if
        Ti > Tj ->
            false;
        true ->
            true
    end.

% ?
clock(Nodes) ->
    [{Node, zero()} || Node <- Nodes].

% return a clock that has been updated
% given that we have received a log message
% from a node at a given time
update(Node, Time, Clock) ->
    List = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    SortedList = lists:keysort(2, List),
    SortedList.

safe(_, []) ->
    true;
% [{_, MsgTime}|T]
safe(Time, Clock) ->
    lists:all(
        fun({_N, T}) ->
            leq(Time, T)
        end,
        Clock
    ).

% if
    %     Time =< MsgTime ->
    %         safe(Time, T);
    %     true ->
    %         false
    % end.
