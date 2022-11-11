-module(node2).

-export([start/1, start/3, test1/0, test2/0]).

-define(Timeout, 10000).
-define(Stabilize, 1000).

% In this node2 module we will add a local store to each node
% and the possibility to add and search for key-value pairs

% ============Staring a Node============

start(Id) ->
    start(Id, nil, []).

start(Id, Peer, Store) ->
    spawn(fun() ->
        init(Id, Peer, Store)
    end).

%Predecessor nil, connect to Successor
init(Id, Peer, Store) ->
    io:format("+=================+~nNode (~p), peer: ~p ~n", [Id, Peer]),
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Store).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("Time out: no response from ~w~n", [Peer])
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

node(Id, Predecessor, Successor, Storage) ->
    receive
        %%Peer needs our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Storage);
        % Peer let's us know it exists
        % Peer becomes our Predecessor or not
        % Handover data
        {notify, New} ->
            {Pred, Store} = notify(New, Id, Predecessor, Storage),
            node(Id, Pred, Successor, Store);
        % Our predecessor asks for our predecessor
        % response: Peer ! {status, {Pkey, Ppid}}
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Storage);
        % Our successor tells us about its predecessor
        % it either stays successor, or its predecessors become our successors
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Storage);
        % When the process receives a stabilize message it will call stabilize/1
        % procedure.
        % The procedure schedule stabilize/1 is called when a node is created
        % Add scheduled stabilization and probing
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Storage);
        % We use probe so to go around the chord and measure roundtrip
        % start probing
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Storage);
        % probe made the roundtrip
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Storage);
        % forward probe
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Storage);
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
                Storage
            ),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(
                Key,
                Qref,
                Client,
                Id,
                Predecessor,
                Successor,
                Storage
            ),
            node(Id, Predecessor, Successor, Storage);
        % It’s a message from a node that has
        % accepted us as their predecessor
        {handover, Elements} ->
            Merged = storage:merge(Storage, Elements),
            node(Id, Predecessor, Successor, Merged);
        status ->
            io:format("Node Id ~w \nPred ~w \nSucc ~w \nContent ~w ~n \n", [
                Id, Predecessor, Successor, Storage
            ]),
            node(Id, Predecessor, Successor, Storage);
        _Msg ->
            io:format("Node ~w received undetermined msg ~w ~n", [Id, _Msg])
    end.

% ============Adding an element============

% To add a new key value we must first determine if our node is the node that
% should take care of the key. A node will take care of all keys from (but not
% including) the identifier of its predecessor to (and including) the identifier
% of itself. If we are not responsible we simply send a add message to our
% successor.

% add key-value pair, return updated storage
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Storage) ->
    case (key:between(Key, Pkey, Id)) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Storage);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Storage
    end.

% ============Lookup procedure============

% The lookup procedure is very similar, we need to do the same test to determine if we are responsible for the key. If so we do a simple lookup in
% the local store and then send the reply to the requester. If it is not our
% responsibility we simply forward the request.

% return {Key, Value} or false
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Storage) ->
    case (key:between(Key, Pkey, Id)) of
        true ->
            Result = storage:lookup(Key, Storage),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

% ============Responsibility============

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

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
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        % have no successor, get a new one
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        % alone in the ring
        {Id, _} ->
            Successor;
        % everything fine
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;
        % new node
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                % new successor
                true ->
                    Xpid ! {request, self()},
                    Pred;
                % We should be our successors predecessor!
                % they have a wrong predecessor now
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

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
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
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
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

% ============Probe Functions============

% use erlang:system time(micro seconds)to get microsecond accuracy

create_probe(Id, Successor) ->
    {_, Spid} = Successor,
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% Ref - original sender, T - time of origin
forward_probe(Ref, T, Nodes, Id, Successor) ->
    {_, Spid} = Successor,
    Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Rtt = erlang:system_time(micro_seconds) - T,
    io:format("Probe roundtrip ~w through ~w ~n", [Rtt, Nodes]).

% ============A local Storage============

% A local storage can be easily implemented as a list of tuples {Key, Value}

% ============Tests============

test1() ->
    Peer1 = test:start(node2),
    timer:sleep(3000),
    test:start(node2, 4, Peer1),
    timer:sleep(8000),
    Peer1 ! probe,
    timer:sleep(1000),
    Keys = test:keys(5000),
    timer:sleep(1000),
    test:add(Keys, Peer1),
    timer:sleep(5000),
    Peer1 ! status,
    test:check(Keys, Peer1),
    timer:sleep(5000),
    Peer1.

test2() ->
    Peer1 = test:start(node2),
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
