-module(filtering).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, check_numbers/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    RedisEntity = redis_manager:get(),
    high_resolution_timer:add_method(fun() -> spawn(?MODULE, check_numbers, [RedisEntity]) end, 700000),
    {ok, RedisEntity}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

check_numbers(RedisEntity) ->
    {ok, BinaryNumberList} = redis_manager:pop_from_list(5000, RedisEntity),
    PrimeNumberList = [binary_to_integer(N) || N <- BinaryNumberList, prime:is_prime(binary_to_integer(N))],
    redis_manager:put_in_set(PrimeNumberList, RedisEntity).
