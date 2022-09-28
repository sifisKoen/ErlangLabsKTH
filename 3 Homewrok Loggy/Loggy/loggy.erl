-module(loggy).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->
        init(Nodes)
    end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    loop(Clock, []).

loop(Clock, OldMessageList) ->
    receive
        {log, From, Time, Msg} ->
            % We create a Message List with all of our messages
            NewMessageList = OldMessageList ++ [{log, From, Time, Msg}],
            % Our Message list sorted
            SortedNewMessageList = lists:keysort(3, NewMessageList),
            % We update our time
            UpdatedTime = time:update(From, Time, Clock),
            % Sent our new Updated List with Times to Safe so to get checked.
            case time:safe(Time, UpdatedTime) of
                true ->
                    loop(UpdatedTime, logInfo(Time, SortedNewMessageList));
                false ->
                    loop(UpdatedTime, NewMessageList)
            end;
        % log(From, Time, Msg),
        % loop();
        stop ->
            ok
    end.

% log(From, Time, Msg) ->
%     io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

logInfo(_, []) ->
    [];
logInfo(Time, [{log, From, MessageTime, Msg} | T]) ->
    if
        MessageTime =< Time ->
            io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
            logInfo(Time, T);
        true ->
            [{log, From, MessageTime, Msg} | T]
    end.
