-module(number_generator_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {
    last_time
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/1, stop/0, generate_number/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(MaxNumber) ->
    gen_server:call(?MODULE, {start, MaxNumber}).
stop() ->
    gen_server:call(?MODULE, stop).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{
        last_time=none}
    }.

handle_call({start, MaxNumber}, _From, State) ->
    CurrentTime = get_micro_sec_time(),
    generate_number(MaxNumber),
    continue(MaxNumber),
    {reply, ok, State#state{last_time = CurrentTime + 333}};
handle_call(stop, _From, State) ->
    {reply, ok, State#state{last_time = none}}.

handle_cast({continue, MaxNumber}, State) ->
    CurrentTime = get_micro_sec_time(),
    if
        State#state.last_time =< CurrentTime ->
            spawn(?MODULE, generate_number, [MaxNumber]),
            continue(MaxNumber),
            NewState = State#state{last_time = State#state.last_time + 333},
            {noreply, NewState};
        State#state.last_time > CurrentTime ->
            continue(MaxNumber),
            {noreply, State}
    end;

handle_cast(continue, _) ->
    {reply, error, "Unexpected value"}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_micro_sec_time() ->
    {_MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Secs * 1000000 + MicroSecs.

continue(MaxNumber) ->
    gen_server:cast(?MODULE, {continue, MaxNumber}).

generate_number(MaxNumber) ->
    RandomNumber = random_generator:uniform(MaxNumber - 1),
    storage_server_my:put_in_list(RandomNumber).