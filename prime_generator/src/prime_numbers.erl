-module(prime_numbers).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run/1]).

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
    find_number(),
    {ok, Args}.

handle_cast(find, State) ->
    {Time, NumberList} = timer:tc(fun() -> storage_server_my_2:get_and_remove_range_from_list() end),
    storage_server_my:put_in_list1(Time),
    values(NumberList),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_number() ->
    gen_server:cast(?MODULE, find).

values({ok, []}) ->
    timer:sleep(100),
    find_number();

values({ok, NumberList}) ->
    spawn(?MODULE, run, [NumberList]),
    find_number().

run(List) ->
    run_check(List).

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
