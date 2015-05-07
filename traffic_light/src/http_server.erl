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

%% обеспечивает взаимодействие с gen-сервером
handle_post(Sock, Path, GenServer) ->
  Length = get_content_length(Sock),
  PostBody = get_body(Sock, Length),
  case Path of
    {abs_path,"/sequence/create"} ->
      send_accept(Sock, getResponseString(my_gen_server:sequence_create(GenServer)));
    {abs_path,"/observation/add"} ->
      Uuid = getDataFromRequest(uuid, PostBody),
      case getDataFromRequest(color, PostBody) of
        "green" ->
          ObserveSections = getDataFromRequest(observe_sections, PostBody),
          send_accept(Sock, getResponseString(my_gen_server:observation_add(GenServer, Uuid, green, ObserveSections)));
        "red" ->
          send_accept(Sock, getResponseString(my_gen_server:observation_add(GenServer, Uuid, red, [])));
        _ ->
          send_accept(Sock, getResponseString({error,{msg, "Undefind color, possible green or red"}}))
      end;
    _ ->
      send_accept(Sock, ""),
      io:fwrite("Unsupported path ~p",[Path])
  end.


handle_get(Sock, Path, GenServer) ->
  case Path of
    {abs_path,"/clear"} ->
      send_accept(Sock, getResponseString(my_gen_server:clean(GenServer)));
    _ ->
      send_accept(Sock, ""),
      io:fwrite("Unsupported path ~p",[Path])
  end.

%% Возврашает нужные поля из тела сообщения
%% цвет, наблюдаемые значения и uuid
getDataFromRequest(color, PostBody) ->
  case (re:run(PostBody, "'color':[^':]*'([^']*)", [{capture, [1], list}])) of
    {match,[Color]} -> Color;
    _ -> {error, {msg, "Incorrect body"}}
  end;
getDataFromRequest(observe_sections, PostBody) ->
  case re:run(PostBody, "'numbers'[^']*'([01]*)[^01]*'([01]*)", [{capture, [1,2], list}]) of
    {match,[NumberLeft, NumberRigth]} ->
      N1 = list_to_integer(NumberLeft, 2),
      N2 = list_to_integer(NumberRigth, 2),
      [N1, N2];
    _ -> {error, {msg, "Incorrect body"}}
  end;
getDataFromRequest(uuid, PostBody) ->
  case (re:run(PostBody, "'sequence'[^']*'([^']*)", [{capture, [1], list}])) of
    {match,[Uuid]} -> Uuid;
    _ -> {error, {msg, "Incorrect body"}}
  end.

%% Формирует тело отвера с результатами работы gen-сервера
getResponseString({ok, {sequence, Uuid}}) ->
  io_lib:format("{'status': 'ok', 'response': {'sequence': '~s'}}~n", [Uuid]);
getResponseString({ok, {start, ResultNumbers, {missing, ErrorSections}}}) ->
  [NumberLeft, NumberRigth] = ErrorSections,
  NumberLeftString = convertIntegerToBitString(NumberLeft),
  NumberRigthString = convertIntegerToBitString(NumberRigth),
  io_lib:format("{'status': 'ok', 'response': {'start': ~p, 'missing': ['~s', '~s']}}~n", [ResultNumbers, NumberLeftString, NumberRigthString]);
getResponseString({ok,{msg, Text}}) ->
  io_lib:format("{'status': 'ok', 'msg': ~s}~n", [Text]);
getResponseString({error,{msg, ErrorText}}) ->
  io_lib:format("{'status': 'error', 'msg': ~s}~n", [ErrorText]).

convertIntegerToBitString(Number) ->
  lists:flatten(io_lib:format("~7..0s", [lists:flatten(io_lib:format("~.2B", [Number]))])).

send_accept(Sock, Mess) ->
  gen_tcp:send(Sock, response(Mess)),
  gen_tcp:close(Sock).

send_unsupported_error(Sock) ->
  gen_tcp:send(Sock, "HTTP/1.1 405 Method not allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html;charset=UTF-8\r\nCache-Control:no-cashe\r\n\r\n"),
  gen_tcp:close(Sock).


handle_request(Sock, GenServer) ->
  {ok, {http_request, Method, Path, Version}} = gen_tcp:recv(Sock, 0),
  case (Method) of
    'POST' -> handle_post(Sock, Path, GenServer);
    'GET' -> handle_get(Sock, Path, GenServer);
    _ -> send_unsupported_error(Sock)
  end.

response(Str) ->
  B = iolist_to_binary(Str),
iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",[size(B), B])).
