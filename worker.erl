% genserver node, restart transient
-module(worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

-record(state, {count = 0}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{count = 0}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(
    {next},
    S = #state{
        count = Count
    }
) ->
    if
        (Count == 0) ->
            driver_serv:done(self());
        true ->
            ok
    end,
    if
        (Count < 10) ->
            Next_Pids = topology_serv:get_all_neighbours(self()),
            lists:foreach(fun(Pid) -> gen_server:cast(Pid, {next}) end, Next_Pids);
        true ->
            ok
    end,
    {noreply, S#state{count = Count + 1}}.
