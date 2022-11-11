-module(node1).

-export([start/1, start/2]).

-define(Timeout, 10000).
-define(Stabilize, 1000).

% ============Staring a Node============

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() ->
        init(Id, Peer)
    end).

% Predecessor nil, connect to Successor
init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
    receive
        % Peer needs our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        % Peer let's us know it exists
        % Peer becomes our Predecessor or not
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        % Our predecessor asks for our predecessor
        % response: Peer ! {status, {Pkey, Ppid}}
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        % Our successor tells us about its predecessor
        % it either stays successor, or its predecessors become our successors
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        % When the process receives a stabilize message it will call stabilize/1
        % procedure.
        % The procedure schedule stabilize/1 is called when a node is created
        % Add scheduled stabilization and probing
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        % probing is to go around the chord and measure roundtrip
        % start probing
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        % probe made the roundtrip
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        % forward probe
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
        status ->
            io:format("Node Id ~w Pred ~w Succ ~w ~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor);
        _Msg ->
            io:format("Node ~w received undetermined msg ~w ~n", [Id, _Msg])
    end.

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
        % all alone in the ring
        {Id, _} ->
            Successor;
        % everything fine
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;
        % new node
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                % new successor!
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

% ============Notify============

% Being notified of a node is a way for a node to make a friendly proposal that
% it might be our proper predecessor
% But we need to have our own investigation

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey, _} ->
            % we do have a new predecessor
            % if Nkey in (Pkey, Id]:
            case key:between(Nkey, Pkey, Id) of
                true ->
                    {Nkey, Npid};
                false ->
                    Predecessor
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
