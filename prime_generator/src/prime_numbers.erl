-module(prime_numbers).
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

init(Args) ->
    RedisEntity = redis_connection_pull:get(),
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
    {Time, {ok, NumberList}} = timer:tc(fun() -> redis_connection_pull:get_and_remove_range_from_list(RedisEntity) end),
%%    io:format("dfdfdf ~p~n", [Time]),
%%    {ok, Srrr} = storage_server_my_2:get_from_list(),
%%    storage_server_my:put_in_list1(Time),
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
    redis_connection_pull:put_in_set(Number, RedisEntity);
save_number({false, _Number}, _RedisEntity) ->
    ok.
