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
    Redis = ?CHILD(redis_manager, worker),
    Filtering = ?CHILD(filtering, worker),
    Generator = ?CHILD(generator, worker),
    RandomGenerator = ?CHILD(random_generator, worker),
    HighResolutionTimer = ?CHILD(high_resolution_timer, worker),
    {ok, { {one_for_one, 5, 10}, [Redis, HighResolutionTimer, RandomGenerator, Filtering, Generator]} }.

