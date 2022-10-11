-module(map).

-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

% ============== Update ==============

% update(berlin, [madrid], [{berlin,[london,paris]}])
% Node = berlin
% Links =[madrid]
% Map = [{berlin, [london,paris]}]
% In this case we want to replace the berlin with london and paris

% Update the Map to reflect that Node has directional links
% to all nodes in the list Links
% So in the Links we pass Nodes
update(Node, Links, Map) ->
    case Map of
        [] ->
            [{Node, Links}];
        [_] ->
            lists:keydelete(Node, 1, Map),
            [{Node, [Links]}];
        _ ->
            io:format("Use list ~n")
    end.

% ============== Reachable ==============

% Returns the list of nodes directly reachable from Node

reachable(Node, Map) ->
    % We check if the first element of the list's tuple is
    % the Node.
    % We use lists:keyfind(Node, 1, Map) so to extract from
    % the list the tuple and pass it to erlang:element
    % so check if the Node is correct.
    % erlang:element(Number, Tuple)
    % lists:keyfind(Key, Number, List)
    case lists:keyfind(Node, 1, Map) of
        false ->
            [];
        {Node, Links} ->
            % erlang:element(2, lists:keyfind(Node, 1, Map))
            Links
    end.

% ============== all_nodes ==============

all_nodes(Map) ->
    % Map = [{Node, [FirstLink, SecondLink]}]
    % [NewMap] = Map,
    % Newmap = {Node, [FirstLink, SecondLink]}
    % ListedMap = tuple_to_list(NewMap),
    % Newmap = [Node, [FirstLink, SecondLink]]
    % [Node, Links] = ListedMap,
    % lists:append(lists:reverse(Links), [Node]).
    TheMap = lists:foldl(
        fun({Nodes, Links}, A) ->
            [Nodes | Links ++ A]
        end,
        [],
        Map
    ),
    % We use usort because we want to remove the doubles
    lists:usort(TheMap).

% {Node, Links} = NewMap,
    % Node = Node
    % Links = [FirstLink, SecondLink]
    % [lists:reverse(Links), Node].
% [erlang:element(2, NewMap), erlang:element(1, NewMap)].
