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

-export([init/1, handle_cast/2,
         terminate/2, code_change/3]).

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

handle_cast(find, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


check_numbers(RedisEntity) ->
    {ok, NumberList} = redis_manager:get_and_remove_range_from_list(RedisEntity),
    run_check(NumberList, RedisEntity).

run_check([], _RedisEntity) ->
    ok;
run_check([Number| NumberList], RedisEntity) ->
    check_and_save_number(Number, RedisEntity),
    run_check(NumberList, RedisEntity).

check_and_save_number(undefined, _RedisEntity) ->
    ok;
check_and_save_number(Number, RedisEntity) ->
    IntNumber = binary_to_integer(Number),
    save_number({prime:is_prime(IntNumber), IntNumber}, RedisEntity).

save_number({true, Number}, RedisEntity) ->
    redis_manager:put_in_set(Number, RedisEntity);
save_number({false, _Number}, _RedisEntity) ->
    ok.