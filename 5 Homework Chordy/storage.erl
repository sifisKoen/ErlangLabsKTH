-module(storage).

-export([create/0, add/3, lookup/2, split/3, merge/2]).

% create() -> create a new store
create() ->
    [].

% add(Key, Value, Store) -> add a key value pair, return the updated store
add(Key, Value, Store) ->
    lists:keystore(Key, 1, Store, {Key, Value}).

% lookup(Key, Store) -> return a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

% The split and merge functions will be used when a new node joins the
% ring and should take over part of the store.

% split(From, To, Store) ->
%   return a tuple {Updated, Rest} where the
%   updated store only contains the key value pairs requested and the rest
%   are found in a list of key-value pairs
split(From, To, Store) ->
    lists:partition(
        fun({K, _}) ->
            key:between(K, From, To)
        end,
        Store
    ).

% merge(Entries, Store) -> add a list of key-value pairs to a store
merge(Entries, Store) ->
    lists:keymerge(1, Entries, Store).
