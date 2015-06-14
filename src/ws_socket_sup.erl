-module(ws_socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
  supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSock]) ->
  ChildSpec = {ws_socket_event, {ws_socket_event, start_link, [LSock]}, temporary, brutal_kill, worker, [ws_socket_event]},
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, { RestartStrategy, [ChildSpec]} }.
