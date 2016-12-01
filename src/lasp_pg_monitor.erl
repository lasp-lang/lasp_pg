-module(lasp_pg_monitor).

-behavior(gen_server).

-export([start_link/0,
         monitor_me/2,
         rm_monitor/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TAB, nuclues_pm_tab).

-record(state, {}).

start_link() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [bag, public, named_table,
                           {write_concurrency, true},
                           {read_concurrency, true}]);
        _ ->
            ok
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

monitor_me(Name, Pid) ->
    _ = ets:insert(?TAB, {Pid, Name}),
    gen_server:cast(?MODULE, {monitor_me, Pid}).

rm_monitor(Name, Pid) ->
    ets:delete_object(?TAB, {Pid, Name}).

init([]) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({monitor_me, Pid}, State) ->
    _ = erlang:monitor(process, Pid),
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _}, S) ->
    _ = process_down(Pid),
    {noreply, S};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal functions

process_down(Pid) when is_pid(Pid) ->
    [begin
         unregister_name(Name, Pid),
         ets:delete(?TAB, Pid)
     end || {_, Name} <- ets:lookup(?TAB, Pid)].


-spec unregister_name(Name :: term(), Pid :: pid()) -> term().
unregister_name(Name, Pid) ->
    lasp_pg:leave(Name, Pid).
