%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
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

get_content_length(Sock) ->
  case gen_tcp:recv(Sock, 0, 60000) of
    {ok, {http_header, _, 'Content-Length', _, Length}} -> list_to_integer(Length);
    {ok, {http_header, _, Header, _, _}} -> get_content_length(Sock)
  end.

get_body(Sock, Length) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, http_eoh} -> inet:setopts(Sock, [{packet, raw}]),
      {ok, Body} = gen_tcp:recv(Sock, Length),
      Body;
    _ -> get_body(Sock, Length)
  end.

handle_post(Sock, Path, GenServer) ->
  Length = get_content_length(Sock),
  PostBody = get_body(Sock, Length),
  case Path of
    {abs_path,"/sequence/create"} ->
      io:fwrite("/sequence/create"),
      Seq = my_gen_server:sequence_create(GenServer),
      io:fwrite("~p",[Seq]);
    {abs_path,"/observation/add"} -> io:fwrite("/observation/add");
    {abs_path,"/clear"} -> io:fwrite("/clear");
    _ -> io:fwrite("Unsupported path ~p",[Path])
  end,
  io:fwrite(PostBody),
  io:fwrite("\n"),
  send_accept(Sock).

send_accept(Sock) ->
  gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html;charset=UTF-8\r\nCache-Control:no-cashe\r\n\r\n"),
  gen_tcp:close(Sock).

send_unsupported_error(Sock) ->
  gen_tcp:send(Sock, "HTTP/1.1 405 Method not allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html;charset=UTF-8\r\nCache-Control:no-cashe\r\n\r\n"),
  gen_tcp:close(Sock).


handle_request(Sock, GenServer) ->
  {ok, {http_request, Method, Path, Version}} = gen_tcp:recv(Sock, 0),
  case (Method) of
    'POST' -> handle_post(Sock, Path, GenServer);
    _ -> send_unsupported_error(Sock)
  end.

response(Str) ->
  B = iolist_to_binary(Str),
iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",[size(B), B])).
