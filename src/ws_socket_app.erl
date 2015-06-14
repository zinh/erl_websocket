-module(ws_socket_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 8080).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  io:format("Start socket~n"),
  {ok, LSock} = gen_tcp:listen(?DEFAULT_PORT, [binary, {active, true}]),
  ws_socket_sup:start_link(LSock).

stop(_State) ->
  ok.
