-module(ws_socket_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 8080).

-record(state, {lsock}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Port = case application:get_env(ws_socket, port) of
    {ok, P} -> P;
    undefined -> ?DEFAULT_PORT
  end,
  {ok, LSock} = gen_tcp:listen(Port, [binary, {active, true}]),
  {ok, Pid} = ws_socket_sup:start_link(LSock),
  {ok, Pid, #state{lsock = LSock}}.

stop(#state{lsock = LSock}) ->
  gen_tcp:stop(LSock),
  ok.
