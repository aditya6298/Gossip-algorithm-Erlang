-module(topo).
-export([get_neighbours/2]).

get_line_neighbours(NumNodes, I, List, Acc) ->
    NeighboursList =
        case I of
            1 ->
                [I + 1];
            NumNodes ->
                [I - 1];
            _ ->
                [I - 1, I + 1]
        end,
    Neighbours = lists:map(fun(X) -> lists:nth(X, List) end, NeighboursList),
    NewAcc = Acc ++ [Neighbours],
    case I of
        NumNodes ->
            NewAcc;
        _ ->
            get_line_neighbours(NumNodes, I + 1, List, NewAcc)
    end.

get_twoD_neighbours(NumNodes, RowCount, I, List, Acc) ->
    NeighboursList =
        case I of
            2 ->
                [I + 1, I + RowCount];
            _ when I == RowCount + 1 ->
                [I - 1, I + RowCount];
            _ when I == NumNodes + 1 ->
                [I - 1, I - RowCount];
            _ when I == NumNodes - RowCount + 2 ->
                [I + 1, I - RowCount];
            _ when I < RowCount + 1 ->
                [I - 1, I + 1, I + RowCount];
            _ when (I > NumNodes - RowCount + 2) and (I < NumNodes + 1) ->
                [I - 1, I + 1, I - RowCount];
            _ when (I - 1 rem RowCount) == 0 ->
                [I + 1, I - RowCount, I + RowCount];
            _ when (I rem RowCount) == 0 ->
                [I - 1, I - RowCount, I + RowCount];
            _ ->
                [I - 1, I + 1, I - RowCount, I + RowCount]
        end,

    Neighbours = lists:map(fun(X) -> lists:nth(X - 1, List) end, NeighboursList),
    NewAcc = Acc ++ [Neighbours],
    MaxNodes = NumNodes + 1,
    case I of
        MaxNodes ->
            NewAcc;
        _ ->
            get_twoD_neighbours(NumNodes, RowCount, I + 1, List, NewAcc)
    end.


get_threeD_neighbours(NumNodes, RowCount, ColCount, I, List, Acc) ->
        NeighboursList =
            case I of
                2 ->
                    [I + 1, I + RowCount, I + ColCount];
                _ when (I - 1) == RowCount ->
                    [I - 1, I + RowCount, I + ColCount];
                _ when (I - 1) == ColCount - RowCount + 1 ->
                    [I + 1, I - RowCount, I + ColCount];
                _ when (I - 1) == ColCount ->
                    [I - 1, I - RowCount, I + ColCount];
                _ when (I - 1) == 1 + ColCount ->
                    [I + 1, I - ColCount, I + RowCount, I + ColCount];
                _ when (I - 1) == RowCount + ColCount ->
                    [I - 1, I - ColCount, I + RowCount, I + ColCount];
                _ when (I - 1) == 2 * ColCount - RowCount + 1 ->
                    [I + 1, I - ColCount, I - RowCount, I + ColCount];
                _ when (I - 1) == 2 * ColCount ->
                    [I - 1, I - ColCount, I - RowCount, I + ColCount];
                _ when (I - 1) == 1 + 2 * ColCount ->
                    [I + 1, I + RowCount, I - ColCount];
                _ when (I - 1) == RowCount + 2 * ColCount ->
                    [I - 1, I + RowCount, I - ColCount];
                _ when (I - 1) == ColCount + 2 * ColCount - RowCount + 1 ->
                    [I + 1, I - RowCount, I - ColCount];
                _ when (I - 1) == ColCount + 2 * ColCount ->
                    [I - 1, I - RowCount, I - ColCount];
                _ when (I - 1) < RowCount ->
                    [I - 1, I + 1, I + RowCount, I + ColCount];
                _ when ((I - 1) > ColCount - RowCount + 1) and ((I - 1) < ColCount) ->
                    [I - 1, I + 1, I - RowCount, I + ColCount];
                _ when ((I - 1) - 1 rem RowCount) == 0 and ((I - 1) < ColCount) ->
                    [I + 1, I - RowCount, I + RowCount, I + ColCount];
                _ when ((I - 1) rem RowCount) == 0 and ((I - 1) < ColCount) ->
                    [I - 1, I - RowCount, I + RowCount, I - ColCount];
                _ when (I - 1) < ColCount ->
                    [I - 1, I + 1, I - RowCount, I + RowCount, I + ColCount];
                _ when ((I - 1) < ColCount + RowCount) and ((I - 1) > 2 * ColCount) ->
                    [I - 1, I + 1, I + ColCount, I - ColCount, I + RowCount];
                _ when ((I - 1) > 2 * ColCount - RowCount + 1) and ((I - 1) < 2 * ColCount) ->
                    [I - 1, I + 1, I - RowCount, I + ColCount, I - ColCount];
                _ when ((I - 1) - 1 rem RowCount) == 0 and ((I - 1) < 2 * ColCount) ->
                    [I + 1, I - RowCount, I + RowCount, I + ColCount, I - ColCount];
                _ when ((I - 1) rem RowCount) == 0 and ((I - 1) < 2 * ColCount) ->
                    [I - 1, I - RowCount, I + RowCount, I + ColCount, I - ColCount];
                _ when ((I - 1) < 2 * ColCount + RowCount) and ((I - 1) > 2 * ColCount) ->
                    [I - 1, I + 1, I + RowCount, I - ColCount];
                _ when ((I - 1) > 3 * ColCount - RowCount + 1) and ((I - 1) < 3 * ColCount) ->
                    [I - 1, I + 1, I - RowCount, I - ColCount];
                _ when ((I - 1) - 1 rem RowCount) == 0 and ((I - 1) < 3 * ColCount) ->
                    [I + 1, I - RowCount, I + RowCount, I - ColCount];
                _ when ((I - 1) rem RowCount) == 0 and ((I - 1) < 3 * ColCount) ->
                    [I - 1, I - RowCount, I + RowCount, I - ColCount];
                _ when ((I - 1) < 3 * ColCount) and ((I - 1) > 2 * ColCount) ->
                    [I - 1, I + 1, I - RowCount, I + RowCount, I - ColCount];
                _ ->
                    [I - 1, I + 1, I - RowCount, I + RowCount, I + ColCount, I - ColCount]
            end,
        Neighbours = lists:map(
            fun(X) ->
                case X of
                    _ when X - 1 > NumNodes ->
                        nil;
                    _ ->
                        lists:nth(X - 1, List)
                end
            end,
            NeighboursList
        ),
        % filter out nils
        NewNeighbours = lists:filter(fun(X) -> X =/= nil end, Neighbours),
        NewAcc = Acc ++ [NewNeighbours],
        MaxNodes = NumNodes + 1,
        case I of
            MaxNodes ->
                NewAcc;
            _ ->
                get_threeD_neighbours(NumNodes, RowCount, ColCount, I + 1, List, NewAcc)
        end.
get_neighbours_helper(List, Topology) ->
    NumNodes = length(List),
    case Topology of
        "line" ->
               
                get_line_neighbours(NumNodes, 1, List, []);
                          
        "full" ->
            lists:map(fun(X) -> lists:delete(X, List) end, lists:seq(1, NumNodes));
        "2d" ->
            RowCount = round(math:sqrt(NumNodes)),
            get_twoD_neighbours(NumNodes, RowCount, 2, List, []);
        "3d" ->
            timer:sleep(10),
            RowCount = round(math:sqrt(NumNodes)),
            get_twoD_neighbours(NumNodes, RowCount, 2, List, []);  
        "imp3d" ->
            timer:sleep(10),
            RowCount = round(math:sqrt(NumNodes)),
            get_twoD_neighbours(NumNodes, RowCount, 2, List, []);   
            
        _ ->
            io:fwrite("Unknown topology~n")
    end.

get_neighbours(List, Topology) ->
    Neighbours = get_neighbours_helper(List, Topology),
    NumNodes = length(List),

    lists:foldl(
        fun(X, Acc) ->
            maps:put(lists:nth(X, List), lists:nth(X, Neighbours), Acc)
        end,
        #{},
        lists:seq(1, NumNodes)
    ).
