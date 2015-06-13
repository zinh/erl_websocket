-module(ws_socket).
-compile(export_all).
-define(MAGIC_STRING, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

start_link(Port) ->
  Pid = spawn(fun() -> 
        {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}]),
        spawn(fun() -> acceptor(LSock) end),
        timer:sleep(infinity)
    end),
  {ok, Pid}.


acceptor(LSock) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  % spawn(fun() -> acceptor(LSock) end),
  handle(Socket).

handle(Socket) ->
  handshake(Socket),
  recv(Socket).

handshake(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Packet} ->
      Headers = get_path(Packet),
      Key = proplists:get_value("Sec-Websocket-Key", Headers),
      AcceptKey = websocket_key(Key),
      Data = [<<"HTTP/1.1 101 Switching Protocols\r\n",
        "Upgrade: websocket\r\n",
        "Connection: Upgrade\r\n",
        "Sec-WebSocket-Accept: ">>,
        AcceptKey,
        <<"\r\n\r\n">>],
      io:format("~s~n", [Data]),
      gen_tcp:send(Socket, Data)
  end.

recv(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Frame} ->
      case Frame of
        <<Fin:1, _:3, Opcode:4, Mask:1, PayloadLen:7, Key:4/binary, Payload/binary>> when PayloadLen < 126 ->
          Message = decode(Key, Payload),
          io:format("~p~n", [Message]),
          send(Socket, Message);
        <<Fin:1, _:3, Opcode:4, Mask:1, PayloadLen:7, Len:32/integer, Key:4/binary, Payload/binary>> when PayloadLen =:= 126 ->
          Message = decode(Key, Payload),
          io:format("~p~n", [Message]),
          send(Socket, Message);
        <<Fin:1, _:3, Opcode:4, Mask:1, PayloadLen:7, Len:64/integer, Key:4/binary, Payload/binary>> when PayloadLen =:= 127 ->
          Message = decode(Key, Payload),
          io:format("~p~n", [Message]),
          send(Socket, Message);
        _ ->
          io:format("Bad format~n")
      end
  end,
  recv(Socket).

send(Socket, Msg) ->
  Len = byte_size(Msg),
  Frame = [<<129, Len>>, Msg],
  gen_tcp:send(Socket, Frame).

decode(Key, Data) when is_binary(Key) ->
  io:format("Key: ~s~n", [Key]),
  Keys = bin_to_list(Key, 1),
  decode(Keys, Data);

decode(Keys, Data) when is_list(Keys) ->
  decode(Keys, Data, []).

decode([H | T], Data, DecodedData) ->
  io:format("H: ~s~n", [H]),
  io:format("Decoded: ~s~n", [DecodedData]),
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

% private
get_path(Packet) ->
  {ok, {http_request, _, {abs_path, Path}, _}, Rest} = erlang:decode_packet(http, Packet, []),
  parse_header(Rest).

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
