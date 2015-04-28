-module(my_gen_server).

-export([start/0, server/1, sequence_create/1, observation_add/2, show_all_sequence/1, stop/1, clean/1, get_session/1]).

start() -> 
	io:format("Starting session server from ~p~n", [self()]),
	InitialState = [],
	spawn(?MODULE, server, [InitialState]).

sequence_create(Pid) ->
	call(Pid, create).

get_session(Pid) ->
  call(Pid, list).

observation_add(Pid, User) ->
  call(Pid, User).

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


		{{observation_add, {color, "green", numbers, Observation_message}, sequence, Uuid}, From, Ref} ->
      [FirstNumber, SecondNumber] = Observation_message,
      PrevObsevations = getPrevObsevations(Uuid, State),
      if
        length(PrevObsevations) == 0 ->
          From ! {reply, Ref, {error, {msg, "The sequence isn't found"}}},
          ?MODULE:server(State);
        true ->
          [{sequence, _,{start, PossibleNumbers, {missing, ErrorSections}}} | _] = PrevObsevations,
          CalculationResult = number:calculatePossibleNumbers(list_to_integer(FirstNumber,2), list_to_integer(SecondNumber,2), PossibleNumbers, ErrorSections),
          From ! {reply, Ref, prepareOutputResult(CalculationResult, PrevObsevations)},
          ?MODULE:server([{sequence, Uuid, CalculationResult} | State])
      end;

    {{observation_add, {color, "red"}, sequence, Uuid}, From, Ref} ->
      PrevObsevations = getPrevObsevations(Uuid, State),
      From ! {reply, Ref, prepareOutputResult([], PrevObsevations)},
      my_gen_server:server(State);

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

prepareOutputResult([], [_ | []]) ->
  {error,{msg, "There isn't enough data"}};
prepareOutputResult([], [LastObsevation | PrevObsevations]) ->
  {sequence, _, {start, _, ErrorSections}} = LastObsevation,
  ResultNumber = length(PrevObsevations),
  {ok, {start, [ResultNumber], ErrorSections}};
prepareOutputResult(CalculationResult, PrevObsevations) ->
  {start, ResultPossibleNumbers, ErrorSections} = CalculationResult,
  if
    length(ResultPossibleNumbers) > 0 ->
      IncreaseNumber = length(PrevObsevations) - 1,
      ResultNumbers = [Number + IncreaseNumber || Number <- ResultPossibleNumbers],
      {ok, {start, ResultNumbers, ErrorSections}};
    length(ResultPossibleNumbers) < 1 -> {error,{msg, "Result not found. Error sequence?"}}
  end.


