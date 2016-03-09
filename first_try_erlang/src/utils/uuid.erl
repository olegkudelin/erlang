%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Формирует UUID
%%% @end
%%% Created : 18. апр 2015 22:41
%%%-------------------------------------------------------------------
-module(uuid).
-author("oleg").

%% API
-export([v4/0, to_string/1]).


v4() ->
  v4(getRandomNumber(48), getRandomNumber(12), getRandomNumber(32), getRandomNumber(30)).

getRandomNumber(Power) ->
  random:uniform(round(math:pow(2, Power))) - 1.

v4(R1, R2, R3, R4) ->
  <<R1:48, 4:4, R2:12, 2:2, R3:32, R4:30>>.

to_string(U) ->
  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
  [TL, TM, THV, CSR, CSL, N].
