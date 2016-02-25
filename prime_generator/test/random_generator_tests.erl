%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Февр. 2016 22:39
%%%-------------------------------------------------------------------
-module(random_generator_tests).
-author("oleg").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).


random_generator_server_test_() ->
  {foreach, fun setup/0, fun cleanup/1, [
    fun(Pid) -> fun() -> server_is_alive(Pid) end end,
    fun() -> fun() -> random_number() end end
  ]}.

setup() ->
  process_flag(trap_exit, true),
  {ok, Pid} = random_generator:start_link(),
  Pid.

server_is_alive(Pid) ->
  ?assertEqual(true, is_process_alive(Pid)).

random_number() ->
  ?assert(random_generator:uniform(20) < 20),
  ?assert(random_generator:uniform(2) == 2),
  ?assert(random_generator:uniform(2) == 2).

cleanup(Pid) ->
  exit(Pid, kill), %% brutal kill!
  ?assertEqual(false, is_process_alive(Pid)).
