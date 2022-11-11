-module(test).

-compile(export_all).

% Used to create the first worker, try:

% For your gms1 use:
% W1 = test:first(1, gms1, 1000).

% For your gms2 use:
% W1 = test:first(1, gms2, 1000).

% For your gms3 use:
% W1 = test:first(1, gms3, 1000).

% W1 = test:first(1, gms4, 1000).

first(N, Module, Sleep) ->
    worker:start(N, Module, random:uniform(256), Sleep).

% Used to create additional workers, try:

% For your gms1 use:
%  test:add(2, gms1, W1, 1000) and
%  test:add(3, gms1, W1, 1000) and ...

% For your gms2 use:
%  test:add(2, gms2, W1, 1000). and
%  test:add(2, gms2, W1, 1000). and ...

% For your gms3 use:
%  test:add(2, gms3, W1, 1000) and
%  test:add(3, gms3, W1, 1000) and ...

%  test:add(2, gms4, W1, 1000) and
%  test:add(3, gms4, W1, 1000) and ...

add(N, Module, Wrk, Sleep) ->
    worker:start(N, Module, random:uniform(256), Wrk, Sleep).

%% To create a number of workers in one go,

% test:more(4, gms4, 1000).

more(N, Module, Sleep) when N > 1 ->
    Wrk = first(1, Module, Sleep),
    Ns = lists:seq(2, N),
    lists:map(fun(Id) -> add(Id, Module, Wrk, Sleep) end, Ns),
    Wrk.

% These are messages that we can send to one of the workers. It will
% multicast it to all workers. They should (if everything works)
% receive the message at the same (logical) time.

freeze(Wrk) ->
    Wrk ! {send, freeze}.

go(Wrk) ->
    Wrk ! {send, go}.

sleep(Wrk, Sleep) ->
    Wrk ! {send, {sleep, Sleep}}.

stop(Wrk) ->
    Wrk ! {send, stop}.
