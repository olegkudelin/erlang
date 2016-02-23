-module(high_resolution_timer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(inner_state, {
    period,
    last_time,
    function
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, add_method/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_method(Callback, Period) ->
    gen_server:call(?MODULE, {add_method, #inner_state{period=Period, last_time = get_micro_sec_time(), function = Callback}}).

check_timer() ->
    gen_server:cast(?MODULE, check_timer).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    check_timer(),
    {ok, []}.

handle_call({add_method, MethodData}, _From, State) ->
    {reply, ok, [MethodData | State]}.

handle_cast(check_timer, State) ->
    NewState = check_timer(State),
    check_timer(),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


check_timer([]) ->
    [];
check_timer([MethodData | MethodDataList]) ->
    CurrentTime = get_micro_sec_time(),
    if
        MethodData#inner_state.last_time =< CurrentTime ->
            Func = MethodData#inner_state.function,
            Func(),
            NewMethodData = MethodData#inner_state{last_time = MethodData#inner_state.last_time + MethodData#inner_state.period},
            [NewMethodData | check_timer(MethodDataList)];
        MethodData#inner_state.last_time > CurrentTime ->
            [MethodData | check_timer(MethodDataList)]
    end.

get_micro_sec_time() ->
    {_MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Secs * 1000000 + MicroSecs.