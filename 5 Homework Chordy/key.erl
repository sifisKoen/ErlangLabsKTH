-module(key).

% We will not use any hash function, instead we will use a random
% number generator, and will be used when a new key is generated.
% A node witch want to join the network will generate a key.

-export([generate/0, between/3, gen/0]).

% Generate returns a random number between 1 and 1.000.000.000
% [ Using a hash function such SHA-1 would give us 160 bits
% and allow us to have human readable names on object. ]
generate() ->
    rand:uniform(1000000000).

% We need to check if the Key is between From and To
% or equal to To, this ia called partly closed internal and
% is denoted (From, To].
% We are dealing with a ring so it could be:
%   From > To and could be From == To
between(Key, From, To) ->
    if
        (From < To) and (To >= Key) and (Key > From) ->
            true;
        (From > To) and ((To >= Key) or (Key > From)) ->
            true;
        (From == To) ->
            true;
        true ->
            false
    end.

gen() ->
    io:format("~w~n", [generate()]),
    gen().
