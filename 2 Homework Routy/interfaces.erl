-module(interfaces).

% ============== Export ==============

-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% ============== New ==============
% return an empty set of interfaces. -> empty list
new() ->
    [].

% ============== Add ==============
% add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

% ============== Remove ==============
% remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

% ============== Remove ==============
% find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false ->
            notfound;
        {_, _, Pid} ->
            {ok, Pid}
    end.

% ============== Remove ==============
% find the reference given a name and return {ok,Ref} or notfound.
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false ->
            notfound;
        {_, Ref, _} ->
            {ok, Ref}
    end.

% ============== Name ==============
% find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        false ->
            notfound;
        {Name, _, _} ->
            {ok, Name}
    end.

% ============== List ==============
% return a list with all names.
list(Intf) ->
    lists:foldl(
        fun({Name, _, _}, L) ->
            [Name | L]
        end,
        [],
        Intf
    ).

% ============== Broadcast ==============
% send the message to all interface pro-cesses.
broadcast(Message, Intf) ->
    lists:map(
        fun({_, _, Pid}) ->
            Pid ! Message
        end,
        Intf
    ).
