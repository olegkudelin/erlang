%%%-------------------------------------------------------------------
%%% @author Kudelin Oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Отвечает за определение возможных чисел исходя из предположения, что часть секций сломана
%%% Принимает работающие секции, и результаты предыдущих расчетов, если они есть
%%% Потом вычисляет возможные числа, использую предыдущую последовательность, предполагая, что предыдущее число на единицу больше, удаляет неверные
%%% и в завершении расчитывает гарантированно сломанные секции
%%% @end
%%% Created : 17. апр 2015 22:17
%%%-------------------------------------------------------------------
-module(number).
-author("oleg").

%% API
-export([calculatePossibleNumbers/4, convertIntegerToBitString/1]).

-compile(export_all).

getNumbersWithPattern() -> [{1,  2#0010010}, {2, 2#1011101},
                    {3, 2#1011011}, {4, 2#0111010},
                    {5, 2#1101011}, {6, 2#1101111},
                    {7, 2#1001010}, {8, 2#1111111},
                    {9, 2#1111011}, {0, 2#1110111}].

%% Рассчитывает возможные чисела и сломанные секции
%%
%% Принимает:
%% LeftSections - левая секция - целое (2#0100110)
%% RigthSections - левая секция - целое (2#0100110)
%% PrefResultNumbers - результаты предыдущего расчеты [1, 23, 34, 45]
%% ErrorSections - сломанные секции [2#0100110, 2#0000110]
%%
%% Отдает
%% {start, [2, 3, 23], missing,  [2#0100110, 2#0000110]}

calculatePossibleNumbers(LeftSections, RigthSections, [], []) ->
  LeftPossibleNumbers = getPossibleNumber(LeftSections, getNumbersWithPattern()),
  RigthPossibleNumbers = getPossibleNumber(RigthSections, getNumbersWithPattern()),
  LeftMissingSection = findMissingSection(LeftSections, LeftPossibleNumbers, 0),
  RigthMissingSection = findMissingSection(RigthSections, RigthPossibleNumbers, 0),
  {start, buildNumbersFromSection(LeftPossibleNumbers, RigthPossibleNumbers), {missing, [LeftMissingSection, RigthMissingSection]}};
calculatePossibleNumbers(LeftSections, RigthSections, PrefResultNumbers, ErrorSections) ->
  Seq = [Number - 1 || Number <- PrefResultNumbers],
  {LeftWaitingNumbers, RigthWaitingNumbers} = splitIntegerNumberBySection(Seq),
  LeftPossibleNumbers = getPossibleNumber(LeftSections, filterNumbersWithPattern(getNumbersWithPattern(), LeftWaitingNumbers)),
  RigthPossibleNumbers = getPossibleNumber(RigthSections, filterNumbersWithPattern(getNumbersWithPattern(), RigthWaitingNumbers)),
  [LeftMissingSection, RigthErrorSection] = ErrorSections,
  LeftMissingSectionNew = findMissingSection(LeftSections, LeftPossibleNumbers, LeftMissingSection),
  RigthErrorSectionNew = findMissingSection(RigthSections, RigthPossibleNumbers, RigthErrorSection),
  PossibleNumbers = buildNumbersFromSection(LeftPossibleNumbers, RigthPossibleNumbers),
  {start, filterByWaitigSeq(PossibleNumbers, Seq), {missing,  [LeftMissingSectionNew, RigthErrorSectionNew]}}.

filterByWaitigSeq(PossibleNumbers, Seq) ->
  [N1 || N1 <- PossibleNumbers, N2 <- Seq, N1 == N2, N1 > 0].

filterNumbersWithPattern(NumbersWithPattern, Numbers) ->
  [{N, Shablon} || {N, Shablon} <- NumbersWithPattern, Number <- Numbers, N == Number ].


getPossibleNumber(NumPattern, AllNumber) ->
  lists:filter(fun({_, Number}) ->
    (NumPattern bor Number) == Number end, AllNumber).

buildNumbersFromSection(LeftNumbers, RigthNumbers) ->
  NumberList = [list_to_integer(integer_to_list(A1) ++ integer_to_list(A2)) || {A1, _} <- LeftNumbers, {A2, _}<-RigthNumbers],
  lists:sort(NumberList).

splitIntegerNumberBySection(Numbers) ->
  LeftNumbers = [Number div 10 rem 10 || Number <- Numbers],
  RigthNumbers = [Number rem 10 || Number <- Numbers],
  {lists:usort(LeftNumbers), lists:usort(RigthNumbers)}.

findMissingSection(NumPattern, PossibleNumbers, OldMissingSection) ->
  WorkingSection = NumPattern,
  MinimumMaibeWorkSection = lists:foldl(fun({_, Number}, Acc) ->
    Number band Acc
  end, 2#1111111, PossibleNumbers),
  MaibeErrorSection = WorkingSection bxor MinimumMaibeWorkSection,
  ErrorSection = MaibeErrorSection bor OldMissingSection,
  ErrorSection.

convertIntegerToBitString(Number) ->
  lists:flatten(io_lib:format("~7..0s", [lists:flatten(io_lib:format("~.2B", [Number]))])).


