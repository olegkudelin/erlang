-module(generator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, generate_number/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(MaxNumber) ->
    ReidsEntity = redis_manager:get(),
    begin_generate(MaxNumber, ReidsEntity),
    {ok, ReidsEntity}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

begin_generate(MaxNumber, State) ->
    high_resolution_timer:add_method(fun() -> spawn(?MODULE, generate_number, [MaxNumber, State]) end, 333).

generate_number(MaxNumber, State) ->
    RandomNumber = random_generator:uniform(MaxNumber),
    redis_manager:put_in_list(RandomNumber, State).