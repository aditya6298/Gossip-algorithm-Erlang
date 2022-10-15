-module(topology_serv).
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    initialise/2,
    get_all_neighbours/1,
    get_random_neighbour/1,
    get_first/0,
    get_mid/0
]).

-record(state, {map = #{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{map = #{}}}.

initialise(List, Topology) ->
    gen_server:cast(topology_serv, {initialise_topo, List, Topology}).

get_all_neighbours(Pid) ->
    gen_server:call(topology_serv, {neighbours, Pid}).

get_random_neighbour(Pid) ->
    gen_server:call(topology_serv, {random_neighbour, Pid}).

get_mid() ->
    gen_server:call(topology_serv, {get_mid}).

get_first() ->
    gen_server:call(topology_serv, {get_first}).

handle_cast({initialise_topo, List, Topology}, S = #state{map = _Map}) ->
    Neighbours = topo:get_neighbours(List, Topology),
    {noreply, S#state{map = Neighbours}};
handle_cast(_Request, State) ->
    io:format("Unknown cast: ~p~n", [_Request]),
    {noreply, State}.

handle_call({random_neighbour, Pid}, _From, S = #state{map = Map}) ->
    {reply, topo:get_random_neighbour(Map, Pid), S#state{map = Map}};
handle_call({neighbours, Pid}, _From, S = #state{map = Map}) ->
    Neighbours = maps:get(Pid, Map),
    {reply, Neighbours, S#state{map = Map}};
handle_call({get_first}, _From, S = #state{map = Map}) ->
    Keys = maps:keys(Map),
    First = hd(Keys),
    {reply, First, S#state{map = Map}};
handle_call({get_mid}, _from, S = #state{map = Map}) ->
    Keys = maps:keys(Map),
    Mid = lists:nth(round(length(Keys) / 2), Keys),
    {reply, Mid, S#state{map = Map}};
handle_call(_Request, _From, State) ->
    io:fwrite("Unknown request~n"),
    {reply, ok, State}.
