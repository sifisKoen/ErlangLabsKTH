-module(vectorTest).
-export([run/2]).

run(Sleep, Jitter) ->
    Log = vectorLoggy:start([john, paul, ringo, george]),
    A = vectorWorker:start(1, john, Log, 13, Sleep, Jitter, 4),
    B = vectorWorker:start(2, paul, Log, 23, Sleep, Jitter, 4),
    C = vectorWorker:start(3, ringo, Log, 36, Sleep, Jitter, 4),
    D = vectorWorker:start(4, george, Log, 49, Sleep, Jitter, 4),
    vectorWorker:peers(A, [B, C, D]),
    vectorWorker:peers(B, [A, C, D]),
    vectorWorker:peers(C, [A, B, D]),
    vectorWorker:peers(D, [A, B, C]),
    timer:sleep(5000),
    vectorLoggy:stop(Log),
    vectorWorker:stop(A),
    vectorWorker:stop(B),
    vectorWorker:stop(C),
    vectorWorker:stop(D).
