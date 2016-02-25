%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Февр. 2016 22:39
%%%-------------------------------------------------------------------
-module(prime_tests).
-author("oleg").

-include_lib("eunit/include/eunit.hrl").
-import(prime, [is_prime/1]).


is_prime_test_() ->
  [
    ?_assert(is_prime(2)),
    ?_assert(is_prime(199)),
    ?_assert(is_prime(900900900900990990990991))
  ].

is_not_prime_test_() ->
  [
    ?_assertNot(is_prime(1)),
    ?_assertNot(is_prime(10)),
    ?_assertNot(is_prime(15))
  ].
