-module(prime_numbers).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, check_numbers/0]).

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
    high_resolution_timer:add_method(fun() -> spawn(?MODULE, check_numbers, []) end, 700000),
%%    find_number(),
    {ok, Args}.

handle_cast(find, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


check_numbers() ->
    {Time, {ok, NumberList}} = timer:tc(fun() -> storage_server_my_2:get_and_remove_range_from_list() end),
%%    io:format("dfdfdf ~p~n", [Time]),
%%    {ok, Srrr} = storage_server_my_2:get_from_list(),
%%    storage_server_my:put_in_list1(Time),
    run_check(NumberList).

run_check([]) ->
    ok;
run_check([Number| NumberList]) ->
    check_and_save_number(Number),
    run_check(NumberList).

check_and_save_number(undefined) ->
    ok;
check_and_save_number(Number) ->
    IntNumber = binary_to_integer(Number),
    save_number({prime:is_prime(IntNumber), IntNumber}).

save_number({true, Number}) ->
    storage_server_my:put_in_set(Number);
save_number({false, _Number}) ->
    ok.
