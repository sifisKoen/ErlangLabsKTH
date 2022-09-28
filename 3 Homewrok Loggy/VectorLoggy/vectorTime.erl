-module(vectorTime).

-export([zero/1, inc/3, merge/2, clock/2, update/3, leq/2, safe/2]).

% Vector -> [{p1, [0,0,0]}, {p2, [0,1,0]},...}]

zero(0) ->
    [];
zero(N) ->
    [0 | zero(N - 1)].

inc(_, [], _) ->
    [];
inc(Index, [H | T], X) ->
    if
        X == Index ->
            [H + 1 | inc(Index, T, X + 1)];
        true ->
            [H | inc(Index, T, X + 1)]
    end.

% zipwith(Combine, List1, List2) -> List3
% compare the two vectors Vi, Vj
leq(Vi, Vj) ->
    ComparisonVectors = lists:zipwith(
        fun(X, Y) ->
            (Y >= X)
        end,
        Vi,
        Vj
    ),
    Result = lists:all(
        fun(E) ->
            E == true
        end,
        ComparisonVectors
    ),
    Result.

merge(Vi, Vj) ->
    Res = leq(Vi, Vj),
    if
        Res ->
            Vj;
        true ->
            Vi
    end.

clock([], _) ->
    [];
% return: [{john,[0,0,0,0]}, {paul,[0,0,0,0]}...]
clock([W | T], ZeroVector) ->
    [{W, ZeroVector} | clock(T, ZeroVector)].

update(Node, TimeVector, Clock) ->
    List = lists:keyreplace(Node, 1, Clock, {Node, TimeVector}),
    List.

safe(_, []) ->
    true;
safe(TimeVector, [{_, MessageVector} | T]) ->
    Res = leq(MessageVector, TimeVector),
    if
        Res -> safe(TimeVector, T);
        true -> false
    end.
