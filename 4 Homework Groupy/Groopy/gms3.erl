-module(gms3).

-compile(export_all).
-define(arghh, 100).
-define(timeout, 1000).

% {msg, N, Msg}
% The messages are chanced and contains the sequence number

% {view, N, Peers, Group}
% A view message with a sequence number.

% Master node
start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, [], [Master]).

% Slave node
start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            random:seed(Rnd, Rnd, Rnd),
            slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves], Group}, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

% ==================Implementation of the Slave==================

% Slave procedure is extended with two arguments
% N -> witch is the expected sequence number of the next message
% Last -> is a copy of the last message received from the leader.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        % We insert this so the slave to accept and ignore duplicate messages
        {msg, I, _} when I < N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, Nr, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Nr + 1, {msg, Nr, Msg}, Slaves, Group);
        {view, Nr, [Leader | Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Nr + 1, Last, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
            ok
    end.

% ==================Implementation of the Leader==================
leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N + 1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self() | Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N + 1, Slaves2, Group2);
        stop ->
            ok
    end.

% ==================Implementation of the Election==================

election(Id, Master, N, Last, Slaves, Group) ->
    io:format("Election ~w: ongoing~n", [Id]),
    Self = self(),
    case Slaves of
        [Self | Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            bcast(Id, Last, Rest),
            Master ! {view, Group},
            leader(Id, Master, N + 1, Rest, Group);
        [Leader | Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(
        fun(Node) ->
            Node ! Msg,
            crash(Id)
        end,
        Nodes
    ).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
