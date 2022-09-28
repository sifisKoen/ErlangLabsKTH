% The worker will wait for some time and will wend a message to one
% of its peers.
% The worker while waiting it is preparing to receive messages from its peers
% so if we run several workers and connect them with each other we will
% have messages randomly being passed between the workers.

% So to keep track of what is happening we send a Log entry to the logger(loggy)
% every time the worker send or receive a message.

% The sleep value will: determine how active the worker is sending messages.
% The litter value will: introduce a random delay between
% the sending of a message and sending of a log  entry.

-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() ->
        init(Name, Logger, Seed, Sleep, Jitter)
    end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    MessageClock = 0,
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, MessageClock);
        stop ->
            ok
    end.

% given the list of peers when the worker was started we could have a
% race condition where a worker is sending messages to workers that have not
% yet been created
peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.
% In case we don't have Clock
% If we have sleep larger than jitter we can take correct output.
loop(Name, Log, Peers, Sleep, Jitter, MessageClock) ->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            NewClockValue = time:inc(Name, time:merge(Time, MessageClock)),

            Log ! {log, Name, NewClockValue, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, NewClockValue);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = time:inc(Name, MessageClock),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, Time)
    end.

% The jitter introduces a slight delay between sending the message to the peer and
% and informing the logger.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
