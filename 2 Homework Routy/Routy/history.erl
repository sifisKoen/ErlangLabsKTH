-module(history).

% ============== Export ==============

-export([new/1, update/3]).

% ============== New ==============
new(Name) ->
    [{Name, inf}].

% ============== Update ==============
% Check if message number N from the Node is old or new
% If it is old then return old but if it new return {new, Updated}
% where Updated is the updated history.
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        false ->
            {new, [{Node, N} | History]};
        {_, NewNumber} ->
            if
                NewNumber < N ->
                    {new, [{Node, N} | lists:keydelete(Node, 1, History)]};
                true ->
                    old
            end
    end.
