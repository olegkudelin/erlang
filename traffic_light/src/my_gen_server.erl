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

%% Стартует сервер
start() -> 
	io:format("Starting session server from ~p~n", [self()]),
	InitialState = [],
	spawn(?MODULE, server, [InitialState]).

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
  call(Pid, {observation_add, {sequence, Uuid, color, red}}).

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
      NewClient = {sequence, Uuid, {start, [], {missing, []}}},
      NewState = [NewClient | State],
      From ! {reply, Ref, {ok, {sequence, Uuid}}},
			?MODULE:server(NewState);


		{{observation_add, {sequence, Uuid, color, green, numbers, Observation_message}}, From, Ref} ->
      Result = prepareOutputResult(Observation_message, getPrevObsevations(Uuid, State)),
      case Result of
        {error, _} ->
          From ! {reply, Ref, Result},
          ?MODULE:server(State);
        {ok, CalculationResult} ->
          From ! {reply, Ref, Result},
          ?MODULE:server([{sequence, Uuid, CalculationResult} | State])
      end;

    {{observation_add, {sequence, Uuid, color, red}}, From, Ref} ->
      Result = prepareOutputResult([], getPrevObsevations(Uuid, State)),
      case Result of
        {error, _} ->
          From ! {reply, Ref, Result},
          ?MODULE:server(State);
        _ ->
          From ! {reply, Ref, Result},
          ?MODULE:server([{sequence, Uuid, stop} | State])
      end;


    {show_all_sequence, From, Ref} ->
      From ! {reply, Ref, State},
      ?MODULE:server(State);

    {clean, From, Ref} ->
      NewState = [],
      From ! {reply, Ref, {ok, {msg, "Clean sequence"}}},
      ?MODULE:server(NewState);

    {stop, From, Ref} ->
      From ! {reply, Ref, {ok}};

		Msg -> io:format("server ~p got message ~p~n", [self(), Msg]),
		?MODULE:server(State)

	end.

getPrevObsevations(Uuid, State) ->
  lists:filter(fun({sequence, Uuid_loop, _}) ->
    Uuid == Uuid_loop end, State).

prepareOutputResult(_, []) ->
  {error,{msg, "The sequence isn't found"}};
prepareOutputResult(_, [{sequence, _, stop} | _]) ->
  {error,{msg, "The red observation should be the last"}};
prepareOutputResult([], [_ | []]) ->
  {error,{msg, "There isn't enough data"}};
prepareOutputResult([], [LastObsevation | PrevObsevations]) ->
  {sequence, _, {start, _, {missing, ErrorSections}}} = LastObsevation,
  ResultNumber = length(PrevObsevations),
  {ok, {start, [ResultNumber], {missing, ErrorSections}}};
prepareOutputResult(Observation_message, PrevObsevations) ->
  [FirstNumber, SecondNumber] = Observation_message,
  [{sequence, _,{start, PossibleNumbers, {missing, ErrorSections}}} | _] = PrevObsevations,
  CalculationResult = number:calculatePossibleNumbers(FirstNumber, SecondNumber, PossibleNumbers, ErrorSections),
  {start, ResultPossibleNumbers, {missing, NewErrorSections}} = CalculationResult,
  if
    length(ResultPossibleNumbers) > 0 ->
      IncreaseNumber = length(PrevObsevations) - 1,
      ResultNumbers = [Number + IncreaseNumber || Number <- ResultPossibleNumbers],
      {ok, {start, ResultNumbers, {missing, NewErrorSections}}};
    length(ResultPossibleNumbers) < 1 -> {error,{msg, "Result not found. Error sequence?"}}
  end.


