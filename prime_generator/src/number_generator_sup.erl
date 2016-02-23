-module(number_generator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
%%    {ok, Host} = application:get_env(redis_host),
%%    {ok, Port} = application:get_env(redis_port),
%%    {ok, DB} = application:get_env(redis_db),
%%    {ok, Password} = application:get_env(redis_password),
%%    Redis = {eredis, {eredis, start_link, [Host, Port, DB, Password]}, permanent, 5000, worker, [eredis]},

    Redis = ?CHILD(redis_connection_pull, worker),
    Prime = ?CHILD(prime_numbers, worker),
    Generator = ?CHILD(number_generator_server, worker),
    RandomGenerator = ?CHILD(random_generator, worker),
    High_resolution_timer = ?CHILD(high_resolution_timer, worker),
    {ok, { {one_for_one, 5, 10}, [Redis, High_resolution_timer, RandomGenerator, Prime, Generator]} }.

