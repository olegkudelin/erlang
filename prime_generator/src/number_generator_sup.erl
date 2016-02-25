-module(number_generator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILD(I, StartArgs), {I, {I, start_link, StartArgs}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link([1000]).

start_link(MaxNumber) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, MaxNumber).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(MaxNumber) ->
    Redis = ?CHILD(redis_manager),
    Filtering = ?CHILD(filtering),
    Generator = ?CHILD(generator, MaxNumber),
    RandomGenerator = ?CHILD(random_generator),
    HighResolutionTimer = ?CHILD(high_resolution_timer),
    {ok, { {one_for_one, 5, 10}, [Redis, HighResolutionTimer, RandomGenerator, Filtering, Generator]} }.

