-module(dijkstra).

% ============== Export ==============

% -export([entry/2, update/4, iterate/3, table/2, route/2]).

-export([table/2, route/2]).

% ============== Entry ==============

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        false ->
            [],
            0;
        % lists:keyfind(Node, 1, Sorted) --> Return the tuple witch
        % contains the Node in the first place of the Sorted
        {Node, NumberOfHopes, _} ->
            NumberOfHopes
    end.

% ============== Replace ==============

replace(Node, N, Gateway, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        % Use "when" just to compare the NumberOfHopes with the N
        % Where NumberOfHopes is the old number of hopes we needed to reach our dest
        % and N is the new number of hopes we need to reach our dest
        % {_, NumberOfHopes, _} when NumberOfHopes < N ->
        %     Sorted;
        {_, NumberOfHopes, _} when NumberOfHopes > N ->
            % We delete the old tuple from the list
            NewList = lists:keydelete(Node, 1, Sorted),
            % We create a new list with the old list - the old tuple
            % That we want to replace and adding the new tuple we
            % want to add
            NewSorted = [{Node, N, Gateway} | NewList],
            % We use sort method to sort again the new created list
            lists:sort(
                fun({_, NumberOfHopes1, _}, {_, NumberOfHopes2, _}) ->
                    NumberOfHopes1 < NumberOfHopes2
                end,
                NewSorted
            );
        false ->
            io:format("There is no entry of :" + Node)
    end.

% ============== Update ==============

update(Node, N, Gateway, Sorted) ->
    % case lists:keyfind(Node, 1, Sorted) of
    %     false ->
    %         [];
    %     {_, NumberOfHopes, _} when NumberOfHopes =< N ->
    %         Sorted;
    %     {_, NumberOfHopes, _} when NumberOfHopes > N ->
    %         % Here if the new number of hopes is less than the old one
    %         % We call the replace method and we replace the old tuple with the new one
    %         replace(Node, N, Gateway, Sorted)
    % end.
    Dist = entry(Node, Sorted),
    if
        Dist > N ->
            replace(Node, N, Gateway, Sorted);
        true ->
            Sorted
    end.

% ============== Iterate ==============

% 1) No more entries in the sorted list = routing table is done
% 2) If the first entry is a dummy with an infinite path to a city
% we know that the rest of the sorted list is also infinite length
% then the given routing table is complete
% 3)
%   a) Take the first entry in the sorted list,
%   b) Find the nodes in the map reachable from this entry
%       and for each of these nodes update the Sorted list
%   c) The entry that you took from the sorted list is added to the
%       routing table

% Sorted --> [{}, {}, {}]
% Map = [{Node, [Link]}]
% []

iterate(Sorted, Map, Table) ->
    case Sorted of
        [] ->
            Table;
        [{_, inf, _} | _] ->
            Table;
        [{Node, NumberOfHopes, Gateway} | RestOfSortedList] ->
            Neighbors = map:reachable(Node, Map),
            NewSortedList = lists:foldl(
                fun(Neighbor, RestOfSortedList2) ->
                    update(Neighbor, NumberOfHopes + 1, Gateway, RestOfSortedList2)
                end,
                RestOfSortedList,
                Neighbors
            ),

            NewTable = [{Node, Gateway} | Table],
            iterate(NewSortedList, Map, NewTable)
    end.

% ============== Table ==============

% Table function takes a list of Gateways [] + Map -> routing table
% With one entry per node in the map
% Creating a routing table by the Gateways ana the Map
% Test:
% dijkstra:table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]).
% Where [paris, madrid] are the Gateways
% And [{madrid,[berlin]}, {paris, [rome,madrid]}] is our map
table(Gateways, Map) ->
    % List the nodes of the map and construct a initial sorted list.
    % This list should have dummy entries for all nodes with the length set to infinity, inf
    % and the gateway to unknown
    % Use the map(Fun, List1) -> List2
    % Fun = fun((A) -> B)
    % List1 = [A]
    % List2 = [B]
    NodesOfTheMap = lists:map(
        fun(Node) -> {Node, inf, unknown} end,
        % filter(Prep, List1) -> List2
        % Prep = fun((Elem :: T) -> boolean()
        % List1 = List2 = [T]
        % Takes a function from As to Bs, and a list of As and produces a
        % list of Bs by applying the function to every element in the list.
        lists:filter(
            % Check it there are Nodes in the Gateways
            % member will return true if a Node matches with a Gateways element
            fun(Node) -> not lists:member(Node, Gateways) end,
            map:all_nodes(Map)
        )
    ),

    % The entries of the gateways should have length zero and gateway set to itself.
    GatewaysOfTheMap = lists:map(
        fun(Node) -> {Node, 0, Node} end,
        Gateways
    ),
    % We create the AllGateways
    AllGateways = GatewaysOfTheMap ++ NodesOfTheMap,

    % When you have constructed this list you can call iterate with an empty table.
    iterate(AllGateways, Map, []).

% ============== Route ==============

route(Node, Table) ->
    % search the routing table and return the gateway suitable to route messages to a node.
    case lists:keysearch(Node, 1, Table) of
        {n, {_, Gateway}} ->
            {ok, Gateway};
        false ->
            notfound
    end.
