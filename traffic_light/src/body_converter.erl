%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Парсит тело запроса и формирует тело ответа
%%% @end
%%% Created : 09. май 2015 17:45
%%%-------------------------------------------------------------------
-module(body_converter).
-author("oleg").

%% API
-export([parseRequestBody/1, getResponseString/1]).

%% Возврашает ожидаемые поля из тела сообщения:
%% цвет, наблюдаемые значения и uuid или ошбку если их нет или задан неверный формат
parseRequestBody(Body) ->
  case rfc4627:decode(Body) of
    {ok, Json, _} ->
      case rfc4627:get_field(Json, "sequence") of
        {ok, BinaryUuid} ->
          Uuid = binary_to_list(BinaryUuid),
          case rfc4627:get_field(Json, "observation") of
            {ok, Observation} ->
              get_observe_number_field(Observation, Uuid);
            not_found ->
              {error, "Cannot get observation"}
          end;
        not_found ->
          {error, "Cannot get Uuid"}
      end;
    {error, _} ->
      {error, "Error parse JSON"}
  end.

get_observe_number_field(Observation, Uuid) ->
  case rfc4627:get_field(Observation, "color") of
    {ok, <<"green">>} ->
      case rfc4627:get_field(Observation, "numbers") of
        {ok, [Number1, Number2]} ->
          N1 = binary_to_integer(Number1, 2),
          N2 = binary_to_integer(Number2, 2),
          {ok, green, Uuid, [N1, N2]};
        {ok, Number} ->
          {error, io_lib:format("Bad observation ~s~n", [Number])};
        not_found ->
          {error, "Cannot get number"}
      end;
    {ok, <<"red">>} ->
      {ok, red, Uuid};
    {ok, Color} ->
      {error, io_lib:format("Unsupported color: ~s~n", [Color])};
    not_found ->
      {error, "Cannot get color"}
  end.

%% Формирует тело ответа с результатами работы в формате JSON
getResponseString({ok, {sequence, Uuid}}) ->
  rfc4627:encode({obj,[{status, ok}, {response, {obj, [{sequence, list_to_bitstring(Uuid)}]} }]});
getResponseString({ok, {start, ResultNumbers, {missing, ErrorSections}}}) ->
  [NumberLeft, NumberRigth] = ErrorSections,
  NumberLeftString = number:convertIntegerToBitString(NumberLeft),
  NumberRigthString = number:convertIntegerToBitString(NumberRigth),
  rfc4627:encode({obj,[{status, ok}, {response, {obj, [{start, ResultNumbers}, {missing, [list_to_bitstring(NumberLeftString), list_to_bitstring(NumberRigthString)]}]} }]});
getResponseString({ok,{msg, Text}}) ->
  rfc4627:encode({obj,[{status, ok}, {msg, list_to_bitstring(Text)}]});
getResponseString({error,{msg, ErrorText}}) ->
  rfc4627:encode({obj,[{status, error}, {msg, list_to_bitstring(ErrorText)}]}).
