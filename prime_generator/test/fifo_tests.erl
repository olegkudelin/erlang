%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Февр. 2016 22:39
%%%-------------------------------------------------------------------
-module(fifo_tests).
-author("oleg").

-include_lib("eunit/include/eunit.hrl").
-import(fifo, [new/0, push/2, next/1, empty/1]).


fifo_test() ->
  Fifo = new(),
  Fifo1 = push(Fifo, 12),
  Fifo2 = push(Fifo1, 13),
  Fifo3 = push(Fifo2, 14),
  {Val1, Fifo4} = next(Fifo3),
  ?assertEqual(12, Val1),
  Fifo5 = push(Fifo4, 15),
  {Val2, Fifo6} = next(Fifo5),
  ?assertEqual(13, Val2),
  {Val3, Fifo7} = next(Fifo6),
  ?assertEqual(14, Val3),
  {Val4, Fifo8} = next(Fifo7),
  ?assertEqual(12, Val4),
  {Val5, _Fifo9} = next(Fifo8),
  ?assertEqual(15, Val5).
