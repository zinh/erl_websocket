-module(ws_socket_event).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-record(state, {lsock, socket, phase=handshake}).
-define(MAGIC_STRING, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").
-define(WSKey, {pubsub,wsbroadcast}).

%% API
start_link(LSock) ->
  gen_server:start_link(?MODULE, [LSock], []).

%% gen_server's callback
init([LSock]) ->
  {ok, #state{lsock = LSock}, 0}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  ws_socket_sup:start_child(),
  {noreply, State#state{socket = Socket}};

% Handshake
handle_info({tcp, Socket, Data}, #state{phase = handshake} = State) ->
  handshake(Socket, Data),
  gproc:reg({p, l, ?WSKey}),
  {noreply, State#state{phase = handshaked}};

handle_info({tcp, _Socket, Frame}, #state{phase = handshaked} = State) ->
  Msg = parse_frame(Frame),
  broadcast(Msg),
  {noreply, State};

handle_info({_Pid, ?WSKey, Msg}, #state{phase = handshaked, socket = Socket} = State) ->
  send(Socket, Msg),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private funtions
handshake(Socket, Packet) ->
  {_Path, Headers} = get_path(Packet),
  Key = proplists:get_value("Sec-Websocket-Key", Headers),
  AcceptKey = websocket_key(Key),
  Data = [<<"HTTP/1.1 101 Switching Protocols\r\n",
    "Upgrade: websocket\r\n",
    "Connection: Upgrade\r\n",
    "Sec-WebSocket-Accept: ">>,
    AcceptKey,
    <<"\r\n\r\n">>],
  gen_tcp:send(Socket, Data).

parse_frame(Frame) ->
  case Frame of
    <<_Fin:1, _:3, _Opcode:4, _Mask:1, PayloadLen:7, Key:4/binary, Payload/binary>> when PayloadLen < 126 ->
      decode(Key, Payload);
    <<_Fin:1, _:3, _Opcode:4, _Mask:1, PayloadLen:7, _Len:32/integer, Key:4/binary, Payload/binary>> when PayloadLen =:= 126 ->
      decode(Key, Payload);
    <<_Fin:1, _:3, _Opcode:4, _Mask:1, PayloadLen:7, _Len:64/integer, Key:4/binary, Payload/binary>> when PayloadLen =:= 127 ->
      decode(Key, Payload);
    _Other ->
      {error, bad_format}
  end.

send(Socket, Msg) ->
  Len = byte_size(Msg),
  Frame = [<<129, Len>>, Msg],
  gen_tcp:send(Socket, Frame).

decode(Key, Data) when is_binary(Key) ->
  Keys = bin_to_list(Key, 1),
  decode(Keys, Data);

decode(Keys, Data) when is_list(Keys) ->
  decode(Keys, Data, []).

decode([H | T], Data, DecodedData) ->
  case Data of
    <<D:1/binary, Rest/binary>> -> 
      decode(T ++ [H], Rest, DecodedData ++ [crypto:exor(H, D)]);
    _ ->
      binary:list_to_bin(DecodedData)
  end.


bin_to_list(Data, Bitlen) ->
  bin_to_list(Data, Bitlen, []).

bin_to_list(Data, Bitlen, List) ->
  case Data of
    <<T:Bitlen/binary, Rest/binary>> ->
      bin_to_list(Rest, Bitlen, List ++ [T]);
    _ ->
      List
  end.

get_path(Packet) ->
  {ok, {http_request, _, {abs_path, Path}, _}, Rest} = erlang:decode_packet(http, Packet, []),
  Headers = parse_header(Rest),
  {Path, Headers}.

parse_header(Packet) ->
  parse_header(Packet, []).

parse_header(Packet, Headers) ->
  case erlang:decode_packet(httph, Packet, []) of 
    {ok, {http_header, _, Key, _, Value}, Rest} ->
      parse_header(Rest, [{Key, Value} | Headers]);
    {ok, http_eoh, _} ->
      Headers;
    {error, _Reason} ->
      Headers
  end.

websocket_key(Key) ->
  HashKey = crypto:hash(sha, Key ++ ?MAGIC_STRING),
  base64:encode(HashKey).

broadcast(Msg) ->
  gproc:send({p, l, ?WSKey}, {self(), ?WSKey, Msg}).
