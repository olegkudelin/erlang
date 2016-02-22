%%%-------------------------------------------------------------------
%%% @author Kudelin Oleg
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Gen сервер, хранит сессии и результаты их наблюдений
%%% @end
%%% Created : 17. апр 2015 22:17
%%%-------------------------------------------------------------------

-module(my_gen_server).

-export([start/0, server/1, sequence_create/1, observation_add/4, show_all_sequence/1, stop/1, clean/1, get_session/1]).

-record(state, {session}).

%% Стартует сервер
start() ->
  Dict = dict:new(),
  spawn(?MODULE, server, [Dict]).

%% Создает новую сессию и возвращет ее идентификатор
sequence_create(Pid) ->
	call(Pid, create).

get_session(Pid) ->
  call(Pid, list).

%% Добавляет и общитывает новое наблюдение
%% возвращает результаты расчетов
observation_add(Pid, Uuid, green, ObserveSections) ->
  call(Pid, {observation_add, {sequence, Uuid, color, green, numbers, ObserveSections}});
observation_add(Pid, Uuid, red, []) ->
  call(Pid, {observation_add, {sequence, Uuid, color, red, numbers, []}}).

show_all_sequence(Pid) ->
  call(Pid, show_all_sequence).

clean(Pid) ->
  call(Pid, clean).

stop(Pid) ->
  call(Pid, stop).

call(Pid, Msg) ->
  Monitor = erlang:monitor(process, Pid),
  Pid ! {Msg, self(), Monitor},
  receive
    {reply, Monitor, Replay} ->
      erlang:demonitor(Monitor, [flush]),
      Replay;
    {'DOWN', Monitor, _, _, ErrorReason} ->
      {error, ErrorReason}
  after 500 ->
    erlang:demonitor(Monitor, [flush]),
    no_reply
  end.

server(State) ->
	receive
		{create, From, Ref} ->
      Uuid = uuid:to_string(uuid:v4()),
      From ! {reply, Ref, {ok, {sequence, Uuid}}},
      io:format("dist ~p~n", [State]),
      ?MODULE:server(dict:store(Uuid, [], State));


		{{observation_add, {sequence, Uuid, color, Color, numbers, Observation_message}}, From, Ref} ->
      case dict:find(Uuid, State) of
        {ok, PrevObservations} ->
          io:format("Sequense fount ok ~p~n", [State]),
          Result = prepareOutputResult(Observation_message, PrevObservations),
          case Result of
            {error, _} ->
              From ! {reply, Ref, Result},
              ?MODULE:server(State);
            {ok, CalculationResult} ->
              {start, ResultPossibleNumbers, {missing, NewErrorSections}} = CalculationResult,
              IncreaseNumber = length(PrevObservations),
              ResultNumbers = [Number + IncreaseNumber || Number <- ResultPossibleNumbers],
              From ! {reply, Ref, {ok, {start, ResultNumbers, {missing, NewErrorSections}}}},
              case Color of
                green ->
                  ?MODULE:server(dict:store(Uuid, [CalculationResult | PrevObservations], State));
                red ->
                  ?MODULE:server(dict:store(Uuid, [stop | PrevObservations], State))
              end
          end;
        error ->
          io:format("Sequense found error ~p~n", [State]),
          From ! {reply, Ref, {error,{msg, "The sequence isn't found"}}},
          ?MODULE:server(State)
      end;

    {show_all_sequence, From, Ref} ->
      From ! {reply, Ref, State},
      ?MODULE:server(State);

    {clean, From, Ref} ->
      From ! {reply, Ref, {ok, {msg, "Clean sequence"}}},
      ?MODULE:server(dict:new());

    {stop, From, Ref} ->
      From ! {reply, Ref, {ok}};

		Msg -> io:format("server ~p got message ~p~n", [self(), Msg]),
		?MODULE:server(State)

	end.

prepareOutputResult(_, [stop | _]) ->
  {error,{msg, "The red observation should be the last"}};
prepareOutputResult([], [_ | []]) ->
  {error,{msg, "There isn't enough data"}};
prepareOutputResult([], [LastObsevation | PrevObsevations]) ->
  {start, _, {missing, ErrorSections}} = LastObsevation,
  ResultNumber = length(PrevObsevations),
  {ok, {start, [ResultNumber], {missing, ErrorSections}}};
prepareOutputResult(Observation_message, PrevObsevations) ->
  [FirstNumber, SecondNumber] = Observation_message,
  io:format("Value ~p~n", [PrevObsevations]),
  case PrevObsevations of
  [] ->
      CalculationResult = number:calculatePossibleNumbers(FirstNumber, SecondNumber, [], []);
  [{start, PossibleNumbers, {missing, ErrorSections}} | _] ->
      CalculationResult = number:calculatePossibleNumbers(FirstNumber, SecondNumber, PossibleNumbers, ErrorSections)
  end,
  {start, ResultPossibleNumbers, {missing, NewErrorSections}} = CalculationResult,
  if
    length(ResultPossibleNumbers) > 0 ->
      {ok, {start, ResultPossibleNumbers, {missing, NewErrorSections}}};
    length(ResultPossibleNumbers) < 1 ->
      {error,{msg, "Result not found. Error sequence?"}}
  end.


