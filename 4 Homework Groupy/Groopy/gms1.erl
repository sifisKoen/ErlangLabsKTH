-module(gms1).

% Find more about compile -> https://www.erlang.org/doc/man/compile.html
-compile(export_all).

% Id -> a unique name, of the node, only used for debugging
% Master -> the process identifier of the application layer
% Slaves -> an ordered list of the process identifiers of all slaves in the group
% Group -> a list of all application layer processes in the group

% ==================Implementation of the Initialization==================

start(Id) ->
    Self = self(),
    {ok,
        spawn_link(fun() ->
            init(Id, Self)
        end)}.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok,
        spawn_link(fun() ->
            init(Id, Grp, Self)
        end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    end.

% ==================Implementation of the Leader==================

% The following messages should be handled from the leader

% {msg, Msg} -> is multicast to all peers.
% Msg -> is send to application layer.

% {join, Wrk, Peer} -> a message from a peer or the master,
% that is request from a node to join the group

leader(Id, Master, Slaves, Group) ->
    receive
        % Receive a multicast message
        {mcast, Msg} ->
            % will send a message to each of the processes.
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self() | Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok
    end.

bcast(_, _, []) ->
    done;
bcast(Id, Msg, [H | T]) ->
    H ! Msg,
    bcast(Id, Msg, T).

% ==================Implementation of the Slave==================

% The messages from the master are the following

% {mcast, Msg} -> a request from its master to multicast a message, the message is forwarded to the leader
% {join, Wkr, Peer} -> a request from the master to allow a new node to join the group, the message is forwarded to the leader
% {msg, Msg} -> a multicast message from the leader. A message Msg is sent to the master
% {view, Peers, Group} -> a multicast message from the leader. A message Msg is send to the master

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader | Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop ->
            ok
    end.
