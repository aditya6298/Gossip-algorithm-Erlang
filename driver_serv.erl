-module(driver_serv).
-behaviour(gen_server).

-export([start_link/4, init/1, handle_call/3, handle_cast/2, done/1]).

-record(state, {num_nodes, topology, algorithm, done_nodes = [], start_time}).

start_link(Num, Topo, Algo, Start) ->
    gen_server:start_link(
        {local, ?MODULE}, ?MODULE, [Num, Topo, Algo, Start], []
    ).

init([NumNodes, Topology, Algorithm, StartTime]) ->
    gen_server:cast(self(), {start}),
    {ok, #state{
        num_nodes = NumNodes, topology = Topology, algorithm = Algorithm, start_time = StartTime
    }}.

done(Pid) ->
    gen_server:cast(driver_serv, {done, Pid}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

add_nodes(NumNodes, Acc) ->
    if
        (NumNodes == 0) ->
            Acc;
        true ->
            % add a node to the supervisor from node
            NodePid = node_supervisor:add_node(),
            add_nodes(NumNodes - 1, [NodePid | Acc])
    end.

handle_cast(
    {start},
    S = #state{
        num_nodes = NumNodes,
        topology = Topology,
        algorithm = Algorithm,
        done_nodes = Done_Pids,
        start_time = _StartTime
    }
) ->
    % start NumNodes nodes
    NodePids = add_nodes(NumNodes, []),
    % print driver_serv pid
    topology_serv:initialise(NodePids, Topology),

    NextPid = topology_serv:get_mid(),
    gen_server:cast(NextPid, {next}),
    {noreply, S#state{
        num_nodes = NumNodes,
        topology = Topology,
        algorithm = Algorithm,
        done_nodes = Done_Pids,
        start_time = _StartTime
    }};
handle_cast(
    {done, Pid},
    S = #state{
        num_nodes = NumNodes,
        topology = Topology,
        algorithm = Algorithm,
        done_nodes = Done_Pids,
        start_time = StartTime
    }
) ->
    New_Done_Pids = [Pid | Done_Pids],
    if
        (length(New_Done_Pids) == NumNodes) ->
            EndTime = os:timestamp(),
            TotalTime = timer:now_diff(EndTime, StartTime) div 1000,
            io:fwrite("Time for convergence: ~pms~n", [TotalTime]),
            io:format("-------- END ~p --------~n", [Algorithm]),
            halt(0);
        true ->
            ok
    end,
    {noreply, S#state{
        num_nodes = NumNodes, topology = Topology, algorithm = Algorithm, done_nodes = New_Done_Pids
    }}.
