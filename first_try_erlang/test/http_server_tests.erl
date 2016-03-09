%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. апр 2015 10:50
%%%-------------------------------------------------------------------
-module(http_server_tests).
-author("oleg").

-include_lib("eunit/include/eunit.hrl").


getDataFromRequest_uuid_test() ->
  Body = "{'observation': {'color': 'red'},'sequence': 'b839e67c-d637-4afc-9241-63943c4fea83'}",
  Uuid = http_server:getDataFromRequest(uuid, Body),
  ?assertEqual(Uuid, "b839e67c-d637-4afc-9241-63943c4fea83").

getDataFromRequest_color_test() ->
  Body = "{'observation': {'color': 'red'},'sequence': 'b839e67c-d637-4afc-9241-63943c4fea83'}",
  Color = http_server:getDataFromRequest(color, Body),
  ?assertEqual(Color, "red").

getDataFromRequest_observe_sections_test() ->
  Body = "{'observation': {'color': 'green'}, 'numbers': ['1110111', '0011101'], 'sequence': 'b839e67c-d637-4afc-9241-63943c4fea83'}",
  Uuid = http_server:getDataFromRequest(observe_sections, Body),
  ?assertEqual(Uuid, [2#1110111, 2#0011101]).