-module(main_sup).
-export([
    start_link/4, init/1, parse_args/1, start/1, start_main_supervisor/4, start_node_supervisor/0
]).
-behaviour(supervisor).

start_link(NumOFNodes, Topo, Algo, Start) ->
    supervisor:start_link(?MODULE, {NumOFNodes, Topo, Algo, Start}).

init({NumOFNodes, Topo, Algo, Start}) ->
    RestartStrategy = {one_for_one, 10, 60},
    ChildSpecification1 =
        {ds, {driver_serv, start_link, [NumOFNodes, Topo, Algo, Start]}, permanent,
            brutal_kill, worker, [driver_serv]},
    ChildSpecification2 =
        {ts, {topology_serv, start_link, []}, permanent, brutal_kill, worker, [topology_serv]},
    ChildSpecification3= 
        {ds, {driver_serv, start_link, [NumOFNodes, Topo, Algo, Start]}, permanent,
    brutal_kill, worker, [driver_serv]},   
    case Algo of
      "gossip"->  
       Children = [ChildSpecification1, ChildSpecification2],
       {ok, {RestartStrategy, Children}};
      "pushsum"->
        timer:sleep(20),
       Children = [ChildSpecification3, ChildSpecification2],
       {ok, {RestartStrategy, Children}};
       _->
        io:fwrite("Algorithm: entered is ~p but algo name can only be gossip or pushsum" , [Algo])
end.

    

% parse_args function to parse command line arguments, there are 3 arguments, the first is number of nodes, the second is topology, and the third is algorithm
parse_args([Nums, Topol, Algo]) ->
    {ok, [Nums, Topol, Algo]}.

start_main_supervisor(Num, Topo, Algo, Start) ->
    main_sup:start_link(Num, Topo, Algo, Start),
    receive
        _ ->
            ok
    end.

start_node_supervisor() ->
    node_supervisor:start_link(),
    receive
        _ ->
            ok
    end.

start(Args) ->
    StartTime = os:timestamp(),
    % get numNodes, topology, algorithm from command line
    {ok, [NumN, Topo, Algo]} = parse_args(Args),
    % print beginning message with =========
    io:format("-------- BEGIN ~p --------~n", [Algo]),
    io:fwrite("NumNodes: ~p, Topology: ~p, Algorithm: ~p~n", [NumN, Topo, Algo]),
    % start the main actor
    spawn(main_sup, start_main_supervisor, [
        list_to_integer(atom_to_list(NumN)),
        atom_to_list(Topo),
        atom_to_list(Algo),
        StartTime
    ]),
    % start the node supervisor
    spawn(main_sup, start_node_supervisor, []).

% to start the program, run the following command:
% erl -noshell -s gossip main 100 full push-sum -s init stop
