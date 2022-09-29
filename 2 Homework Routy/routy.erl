-module(routy).

%% API
-export([start/2, stop/1, init/1, router/6]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = interfaces:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = history:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    receive
        %%  add a new node to interfaces
        {add, Node, Pid} ->
            Ref = erlang:monitor(process, Pid),
            Intf1 = interfaces:add(Node, Ref, Pid, Intf),
            Message = {links, Name, N, interfaces:list(Intf1)},
            interfaces:broadcast(Message, Intf1),
            io:format("Routy: added an interface: ~w~n", [Intf1]),
            router(Name, N, Hist, Intf1, Table, Map);
        %%  remove a node from interfaces
        {remove, Node} ->
            {ok, Ref} = interfaces:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = interfaces:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        %%  handle a DOWN message from a died node
        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = interfaces:name(Ref, Intf),
            io:format("~w: exit received from ~w~n", [Name, Down]),
            Intf1 = interfaces:remove(Down, Intf),
            io:format("Routy: removed an interfaces: ~w~n", [Intf1]),
            router(Name, N, Hist, Intf1, Table, Map);
        %%  add a new node to interfaces
        status ->
            io:format("~w received a status call ~n", Name),
            router(Name, N, Hist, Intf, Table, Map);
        %%  pretty-print of the state
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        %%  handle link-state messages
        {links, Node, R, Links} ->
            %%    check whether a message is new or old
            case history:update(Node, R, Hist) of
                {new, Hist1} ->
                    interfaces:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        update ->
            Table1 = dijkstra:table(interfaces:list(Intf), Map),
            io:format("~w: Table updated ~n", [Table1]),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast ->
            Message = {links, Name, N, interfaces:list(Intf)},
            interfaces:broadcast(Message, Intf),
            io:format("~w: Sent a message to ~w~n", [Name, Intf]),
            router(Name, N + 1, Hist, Intf, Table, Map);
        {route, Name, From, Message} ->
            io:format("~w: received message ~w ~n", [Name, Message]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message} ->
            io:format("~w: routing message (~w)", [Name, Message]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case interfaces:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                    end;
                notfound ->
                    ok
            end,
            router(Name, N, Hist, Intf, Table, Map);
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        stop ->
            ok
    end.
