-module(dbyr_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_monitor/1,
         start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_monitor(WebsocketPid) ->
    {ok, Child} = supervisor:start_child(?MODULE, [WebsocketPid]),
    Child.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(dbyr_monitor, worker)]} }.

