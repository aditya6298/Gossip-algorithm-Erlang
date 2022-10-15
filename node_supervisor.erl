-module(node_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1, add_node/0]).

start_link() ->
    {ok, Pid} = supervisor:start_link(
        {local, ?MODULE},
        ?MODULE,
        []
    ),
    {ok, Pid}.

init([]) ->
    RestartStrategy = {simple_one_for_one, 10, 60},
    ChildSpec = {worker, {worker, start_link, []}, permanent, brutal_kill, worker, [worker]},
    Children = [ChildSpec],
    {ok, {RestartStrategy, Children}}.

add_node() ->
    {ok, ChildPid} = supervisor:start_child(node_supervisor, []),
    ChildPid.
