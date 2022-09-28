-module(vectorWorker).
-export([start/7, stop/1, peers/2]).

start(Index, Name, Logger, Seed, Sleep, Jitter, NumberP) ->
    spawn_link(fun() ->
        init(
            Index,
            Name,
            Logger,
            Seed,
            Sleep,
            Jitter,
            NumberP
        )
    end).

stop(Worker) ->
    Worker ! stop.

init(Index, Name, Log, Seed, Sleep, Jitter, NumberP) ->
    random:seed(Seed, Seed, Seed),
    ZeroVector = vectorTime:zero(NumberP),
    receive
        {peers, Peers} ->
            loop(Index, Name, Log, Peers, Sleep, Jitter, ZeroVector);
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Index, Name, Log, Peers, Sleep, Jitter, MessageClock) ->
    Wait = random:uniform(Sleep),
    receive
        {msg, TimeVector, Msg} ->
            NewVector = vectorTime:inc(Index, vectorTime:merge(TimeVector, MessageClock), 1),

            Log ! {log, Name, NewVector, {received, Msg}},
            loop(Index, Name, Log, Peers, Sleep, Jitter, NewVector);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = vectorTime:inc(Index, MessageClock, 1),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Time, Message},
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Index, Name, Log, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
