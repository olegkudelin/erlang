%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Веб-сервер
%%% принимает запросы и передает их gen серверу, возвращет результат его работы
%%% @end
%%% Created : 25. апр 2015 0:00
%%%-------------------------------------------------------------------
-module(http_server).
-author("oleg").
%% API
-export([start/1, loop/2, handle_request/2]).

start(Port) ->
  {ok, Sock} = gen_tcp:listen(Port, [list, {active, false}, {packet, http}]),
  GenServer = my_gen_server:start(),
  ?MODULE:loop(Sock, GenServer).

loop(Sock, GenServer) ->
  {ok, Conn} = gen_tcp:accept(Sock),
  spawn(?MODULE, handle_request, [Conn, GenServer]),
  ?MODULE:loop(Sock, GenServer).

%% определяет из заголовка длинну тела сообщения
get_content_length(Sock) ->
  case gen_tcp:recv(Sock, 0, 60000) of
    {ok, {http_header, _, 'Content-Length', _, Length}} -> list_to_integer(Length);
    {ok, {http_header, _, Header, _, _}} -> get_content_length(Sock)
  end.

%% возвращает тело сообщения
get_body(Sock, Length) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, http_eoh} -> inet:setopts(Sock, [{packet, raw}]),
      {ok, Body} = gen_tcp:recv(Sock, Length),
      Body;
    _ -> get_body(Sock, Length)
  end.

%% обрабатывает POST запросы, обеспечивает взаимодействие с gen-сервером
handle_post(Sock, Path, GenServer) ->
  case Path of
    {abs_path,"/sequence/create"} ->
      send_accept(Sock, body_converter:getResponseString(my_gen_server:sequence_create(GenServer)));
    {abs_path,"/observation/add"} ->
      Length = get_content_length(Sock),
      PostBody = get_body(Sock, Length),
      case body_converter:parseRequestBody(PostBody) of
        {ok, green, Uuid, ObserveSections} ->
          send_accept(Sock, body_converter:getResponseString(my_gen_server:observation_add(GenServer, Uuid, green, ObserveSections)));
        {ok, red, Uuid} ->
          send_accept(Sock, body_converter:getResponseString(my_gen_server:observation_add(GenServer, Uuid, red, [])));
        {error, Message} ->
          send_accept(Sock, body_converter:getResponseString({error,{msg, Message}}));
        _ ->
          send_accept(Sock, body_converter:getResponseString({error,{msg, "Unsupported content"}}))
      end;
    {abs_path, AbsolutePath} ->
      send_accept(Sock, body_converter:getResponseString({error,{msg, io_lib:format("Unsupported function: ~s", [AbsolutePath])}}))
  end.


handle_get(Sock, Path, GenServer) ->
  case Path of
    {abs_path,"/clear"} ->
      send_accept(Sock, body_converter:getResponseString(my_gen_server:clean(GenServer)));
    {abs_path, AbsolutePath} ->
      send_accept(Sock, body_converter:getResponseString({error,{msg, io_lib:format("Unsupported function: ~s", [AbsolutePath])}}))
  end.

send_accept(Sock, Mess) ->
  gen_tcp:send(Sock, response_ok(Mess)),
  gen_tcp:close(Sock).

handle_request(Sock, GenServer) ->
  {ok, {http_request, Method, Path, _}} = gen_tcp:recv(Sock, 0),
  case (Method) of
    'POST' -> handle_post(Sock, Path, GenServer);
    'GET' -> handle_get(Sock, Path, GenServer);
    _ -> send_accept(Sock, body_converter:getResponseString({error,{msg, io_lib:format("Unsupported method: ~s",[Method])}}))
  end.

response_ok(Str) ->
  B = iolist_to_binary(Str),
  iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: application/json\nContent-Length: ~p\n\n~s",[size(B), B])).
