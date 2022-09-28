-module(vectorLoggy).

-export([start/1, stop/1, createQueue/3]).

start(Nodes) ->
    spawn_link(fun() ->
        init(Nodes)
    end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = vectorTime:clock(Nodes, vectorTime:zero(length(Nodes))),
    loop(Clock, []).

createQueue([], E, Flag) ->
    if
        Flag ->
            [];
        true ->
            [E]
    end;
createQueue([H | T], E, Flag) ->
    case E of
        {_, _, InsertV, _} ->
            case H of
                {_, _, V, _} ->
                    Res = vectorTime:leq(InsertV, V),
                    if
                        Res and (Flag == false) ->
                            [E, H | createQueue(T, E, true)];
                        true ->
                            [H | createQueue(T, E, Flag)]
                    end
            end
    end.
loop(Clock, OldMessageList) ->
    receive
        {log, From, Time, Msg} ->
            MessageList = createQueue(OldMessageList, {log, From, Time, Msg}, false),
            % The updated lists:
            List = vectorTime:update(From, Time, Clock),
            case vectorTime:safe(Time, List) of
                true ->
                    loop(List, logInfo(Time, MessageList));
                false ->
                    loop(List, MessageList)
            end;
        stop ->
            ok
    end.

logInfo(_, []) ->
    [];
logInfo(Time, [{log, From, MessageTime, Msg} | T]) ->
    if
        MessageTime =< Time ->
            io:format("log: ~w~w~p~n", [Time, From, Msg]),
            logInfo(Time, T);
        true ->
            [{log, From, MessageTime, Msg} | T]
    end.
