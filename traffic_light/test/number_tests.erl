%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. апр 2015 19:06
%%%-------------------------------------------------------------------
-module(number_tests).
-author("oleg").

-include_lib("eunit/include/eunit.hrl").

splitIntegerNumberBySection_test() ->
  Number = number:splitIntegerNumberBySection([0, 17, 60, 68, 80, 28, 52]),
  ?assertEqual(Number, {[0, 1, 2, 5, 6, 8], [0, 2, 7, 8]}).

calculatePossibleNumbers_startValue_test() ->
  Number = number:calculatePossibleNumbers(2#1111111, 2#1111111, [], []),
  ?assertEqual(Number, {start,[88],{missing,[0,0]}}).

calculatePossibleNumbers_missingSection_test() ->
  Number = number:calculatePossibleNumbers(2#0000110, 2#1110111, [], []),
  ?assertEqual(Number, {start,[0, 08, 60, 68, 80, 88],{missing,[2#1100001,2#0000000]}}).

calculatePossibleNumbers_usingPrevResult_test() ->
  Number = number:calculatePossibleNumbers(2#0000110, 2#0001010, [0, 08, 60, 68, 80, 88], [2#1100001,2#0000000]),
  ?assertEqual(Number, {start,[7,67,87],{missing,[2#1100001,2#1000000]}}).

filterNumbersWithPattern_test() ->
  NumbersWithPattern = number:filterNumbersWithPattern([{1,  2#0010010},{4, 2#0111010},
                              {5, 2#1101011}, {7, 2#1001010},
                              {8, 2#1111111}, {9, 2#1111011}],
    [1,5,9]),
  ?assertEqual(NumbersWithPattern, [{1,  2#0010010}, {5, 2#1101011}, {9, 2#1111011}]).

%% getPossibleNumber_test() ->
%%   number:getPossibleNumber()