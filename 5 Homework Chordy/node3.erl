-module(node3).

-define(Stabilize, 500).
-define(Timeout, 1000).

-export([start/1, start/2, stabilize/1, test1/0, test2/0]).

% In this node2 module we will add a local store to each node
% and the possibility to add and search for key-value pairs

% ============Staring a Node============

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() ->
        init(Id, Peer)
    end).

%Predecessor nil, connect to Successor
init(Id, Peer) ->
    Predecessor = nil,
    Next = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    Store = storage:create(),
    node(Id, Predecessor, Successor, Store, Next).

connect(Id, nil) ->
    Ref = monitor(self()),
    {ok, {Id, Ref, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Ref = monitor(Peer),
            {ok, {Skey, Ref, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n", [])
    end.

% ============The Node============

% That only handles a growing ring
% A node needs to have the following properties:
%  1) Key
%  2) Predecessor
%  3) Successor
% We need to know tha key values of both predecessor and successor
% This will be represented like: {Key, Pid}

% The messages we need to maintain in the ring are:
%  1) {key, Qref, Peer} -> a peer needs to know our key
%  2) {notify, New} -> a new node informs us of its existence
%  3) {request, Peer} -> s predecessor needs to know our predecessor
%  4) {status, Pred} -> our successor informs us about its predecessor

% We need to make the things asynchronous

node(Id, Predecessor, Successor, Store, Next) ->
    receive
        %Peer needs our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);
        %a new node appeared
        {notify, New} ->
            {Pred, Store2} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store2, Next);
        % Our predecessor asks for our predecessor
        % response: Peer ! {status, {Pkey, Ppid}}
        %a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        %our successor informs us about its predecessor
        {status, Pred, Nx} ->
            {Succ, Next2} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Store, Next2);
        % When the process receives a stabilize message it will call stabilize/1
        % procedure.
        % The procedure schedule stabilize/1 is called when a node is created
        % Add scheduled stabilization and probing
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        % We use probe so to go around the chord and measure roundtrip
        % start probing
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        % probe made the roundtrip
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        % forward probe
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        % The procedures will need information about the predecessor and successor in order to
        % determine if the message is actually for us or if it should be forwarded.
        % add storage
        % The Qref parameters will be used to tag
        % the return message to the Client.
        % This allows the client to identify
        % the reply message and makes it
        % easier to implement the client.
        {add, Key, Value, Qref, Client} ->
            Added = add(
                Key,
                Value,
                Qref,
                Client,
                Id,
                Predecessor,
                Successor,
                Store
            ),
            node(Id, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client} ->
            lookup(
                Key,
                Qref,
                Client,
                Id,
                Predecessor,
                Successor,
                Store
            ),
            node(Id, Predecessor, Successor, Store, Next);
        % Delegate responsibility to a new node.
        % It’s a message from a node that has
        % accepted us as their predecessor
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);
        {'DOWN', Ref, process, _, _} ->
            io:format("Got DOWN msg ~n", []),
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);
        %To check DHT works
        status ->
            io:format("Node: ~w   Predecessor: ~w Successor: ~w Next: ~w  Storage: ~w~n", [
                Id, Predecessor, Successor, Next, Store
            ]),
            node(Id, Predecessor, Successor, Store, Next);
        stop ->
            ok
    end.

% ============Adding an element============

% To add a new key value we must first determine if our node is the node that
% should take care of the key. A node will take care of all keys from (but not
% including) the identifier of its predecessor to (and including) the identifier
% of itself. If we are not responsible we simply send a add message to our
% successor.

% add key-value pair, return updated storage
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client}
    end.

% ============Lookup procedure============

% The lookup procedure is very similar, we need to do the same test to determine if we are responsible for the key. If so we do a simple lookup in
% the local store and then send the reply to the requester. If it is not our
% responsibility we simply forward the request.

% return {Key, Value} or false
lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid ! {add, Key, Qref, Client}
    end.

% ============Responsibility============

handover(Id, Store, Nkey, Npid) ->
    {Updated, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Updated.

% ============Stabilize============

% The periodic stabilize procedure will consist of a node sending
% a {request, self()} message ot its successor and then expecting
% a {status, Pred} in return.
% When it knows the predecessor if its successor it can check
%   1) If the ring is stable or
%   2)  Ff the successor needs to be notifies  about its existence through
% a {notify, {Id, self()} message

% Pred -> is ours successors current predecessor
%   1) If this i nil we should of course inform it about our existence
%   2) If it is pointing back to us we don't have to do anything
%   3) If it is pointing to itself we should of course notify it about our existence

% If the key of the predecessor of our successor (Xkey)
% is between us and our successor we should of course adopt this node as our
% successor and run stabilization again. If we should be in between the nodes
% we inform our successor of our existence.

% Stabilize procedure is simply send a request message to its successor

% Input vars:
% Pred = Current predecessor of the successor
% Id = Id of the current node
% Successor = Successor of the current node
stabilize(Pred, Next, Id, Successor) ->
    %%  io:format("Starting stab: Pred - ~w, Id - ~w Succ - ~w Next  ~w ~n", [Pred, Id, Successor, Next]),
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Id, _} ->
            {Successor, Next};
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                % new successor
                true ->
                    % adopt this node as our successor
                    Xpid ! {request, self()},
                    drop(Sref),
                    Xref = monitor(Xpid),
                    self() ! stabilize,
                    {{Xkey, Xref, Xpid}, Successor};
                false ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next}
            end
    end.

stabilize({_, _, Spid}) ->
    %io:format("Self request ~n", []),
    Spid ! {request, self()}.

% ============Request============

% The stabilize procedure must be done with regular internals so that new
% nodes are quickly linked into the ring.
% This can be arranged by stating a TIMER that sends a stabilize message
% after a specific time.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

% The request message is picked up by a
% process and the request/2 procedure is called
% We should of course only inform the peer that sent the
% request about or predecessor
request(Peer, Predecessor, {Nkey, _, Npid}) ->
    case Predecessor of
        nil ->
            Msg = {status, Predecessor, {Nkey, Npid}},
            %%      io:format("Sent status Msg ~w~n", [Msg]),
            Peer ! Msg;
        {Pkey, _, Ppid} ->
            %%      io:format("Sent status with Pred:  ~w~n", [{Pkey, Ppid}]),
            Peer ! {status, {Pkey, Ppid}, {Nkey, Npid}}
    end.

% ============Notify2============

% Being notified of a node is a way for a node to make a friendly proposal that
% it might be our proper predecessor
% But we need to have our own investigation

% Handling of a notify message could mean that we have to give
% part of a store away; we need to pass the store as an argument also return
% a tuple {Predecessor, Store}.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            Npid ! {status, {Nkey, Nref, Npid}},
            {{Nkey, Nref, Npid}, Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    drop(Pref),
                    Nref = monitor(Npid),
                    Npid ! {status, {Nkey, Nref, Npid}},
                    {{Nkey, Nref, Npid}, Keep};
                false ->
                    Npid ! {status, Predecessor},
                    {Predecessor, Store}
            end
    end.

% ============Probe Functions============

% use erlang:system time(micro seconds)to get microsecond accuracy

create_probe(Id, {_, Pid}) ->
    io:format("New Probe~wn", [Id]),
    Pid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% Ref - original sender, T - time of origin
forward_probe(Ref, T, Nodes, Id, {_, Pid}) ->
    io:format("Forward Probe~wn", [Ref]),
    Pid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Tdiff = erlang:system_time(micro_seconds) - T,
    io:format("Remove Probe [~w] from nodes: ~wn", [Tdiff, Nodes]),
    Tdiff.

% ============Monitoring============

% {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),

monitor(Pid) ->
    io:format("Started to monitor a node ~w~n", [Pid]),
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Pid) ->
    erlang:demonitor(Pid, [flush]).

down(Ref, {Key, Ref, _}, Successor, Next) ->
    io:format("Predecessor with key ~w died. ~n", [Key]),
    {nil, Successor, Next};
down(Ref, Predecessor, {Key, Ref, _}, {Nkey, Npid}) ->
    Nref = monitor(Npid),
    self() ! stabilize,
    io:format("Successor with key ~w died. ~n", [Key]),
    {Predecessor, {Nkey, Nref, Npid}, nil}.

% ============Tests============

test1() ->
    Peer1 = test:start(node3),
    timer:sleep(2000),
    test:start(node3, 4, Peer1),
    timer:sleep(8000),
    Peer1 ! probe,
    timer:sleep(1000),
    Keys = test:keys(1000),
    timer:sleep(1000),
    test:add(Keys, Peer1),
    timer:sleep(5000),
    Peer1 ! status,
    test:check(Keys, Peer1),
    timer:sleep(5000),
    PeerExtra = start(nodeExtra, Peer1),
    timer:sleep(8000),
    Peer1 ! probe,
    PeerExtra ! stop,
    timer:sleep(8000),
    Peer1 ! probe,
    Peer1.

test2() ->
    Peer1 = test:start(node3),
    timer:sleep(4000),
    Peer1 ! probe,
    timer:sleep(1000),
    Keys = test:keys(5000),
    timer:sleep(3000),
    test:add(Keys, Peer1),
    timer:sleep(5000),
    Peer1 ! status,
    test:check(Keys, Peer1),
    timer:sleep(5000),
    Peer1.
