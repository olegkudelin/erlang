-module(generator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/1, stop/0, generate_number/2]).

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
    ReidsEntity = redis_manager:get(),
    {ok, ReidsEntity}.

handle_call({start, MaxNumber}, _From, State) ->
    high_resolution_timer:add_method(fun() -> spawn(?MODULE, generate_number, [MaxNumber, State]) end, 333),
    {reply, ok, State}.

handle_cast(continue, _) ->
    {reply, error, "Unexpected value"}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

generate_number(MaxNumber, State) ->
    RandomNumber = random_generator:uniform(MaxNumber - 1),
    redis_manager:put_in_list(RandomNumber, State).