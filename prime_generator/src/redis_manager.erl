-module(redis_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


-record(connection_params, {
    host,
    port,
    db,
    password
}).

-record(structures_names, {
    queue_key,
    result_set_key
}).

-record(redis_entity, {
    redis_pid,
    structures_names
}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get/0, pop_from_list/1, pop_from_list/2, put_in_set/2, put_in_list/2, clear_list/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    {ok, Host} = application:get_env(redis_host),
    {ok, Port} = application:get_env(redis_port),
    start_link(Host, Port).

start_link(Host, Port) ->
    {ok, DB} = application:get_env(redis_db),
    start_link(Host, Port, DB).

start_link(Host, Port, DB) ->
    {ok, Password} = application:get_env(redis_password),
    start_link(Host, Port, DB, Password).

start_link(Host, Port, DB, Password) ->
    RedisConnectionTuple = #connection_params{
        host = Host,
        port = Port,
        db = DB,
        password = Password
    },
    RedisData = get_structures_names(),
gen_server:start_link({local, ?MODULE}, ?MODULE, {redis_connection, RedisConnectionTuple, structures_names, RedisData}, []).

get() ->
    gen_server:call(?MODULE, get).


pop_from_list(StateEntity) ->
    eredis:q(StateEntity#redis_entity.redis_pid, ["RPOP", StateEntity#redis_entity.structures_names#structures_names.queue_key]).

pop_from_list(Count, RedisConnection) when Count > 0 ->
    {ok, Data} = eredis:q(RedisConnection#redis_entity.redis_pid, ["LRANGE", RedisConnection#redis_entity.structures_names#structures_names.queue_key, 0, Count]),
    {ok, <<"OK">>} = remove_from_list(erlang:length(Data), RedisConnection),
    {ok, Data}.

remove_from_list(Count, RedisConnection) ->
    eredis:q(RedisConnection#redis_entity.redis_pid, ["LTRIM", RedisConnection#redis_entity.structures_names#structures_names.queue_key, Count, -1]).

put_in_set(Numbers, StateEntity) when is_list(Numbers) ->
    ParameterList = ["SADD" | [StateEntity#redis_entity.structures_names#structures_names.result_set_key | Numbers]],
    eredis:q(StateEntity#redis_entity.redis_pid, ParameterList);
put_in_set(Number, RedisConnection) ->
    eredis:q(RedisConnection#redis_entity.redis_pid, ["SADD", RedisConnection#redis_entity.structures_names#structures_names.result_set_key, Number]).


put_in_list(Number, StateEntity) ->
    eredis:q(StateEntity#redis_entity.redis_pid, ["RPUSH", StateEntity#redis_entity.structures_names#structures_names.queue_key, Number]).

clear_list(StateEntity) ->
    eredis:q(StateEntity#redis_entity.redis_pid, ["DEL", StateEntity#redis_entity.structures_names#structures_names.queue_key]).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    RedisList = build_redis_list(Args, fifo:new(), 5),
    {ok, RedisList}.

handle_call(get, _From, State) ->
    {Redis, NewState} = fifo:next(State),
    {reply, Redis, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

build_redis_list(_RedisConnection, Fifo, 0) ->
    Fifo;
build_redis_list(Args, Fifo, ElementCount) when ElementCount > 0 ->
    {redis_connection, RedisConnectionTuple, structures_names, RedisData} = Args,
    StateEntity = #redis_entity{
        redis_pid = create_redis_connection(RedisConnectionTuple),
        structures_names = RedisData},
    build_redis_list(Args, fifo:push(Fifo, StateEntity), ElementCount - 1).

create_redis_connection(Args) ->
    {ok, Redis} = eredis:start_link(Args#connection_params.host, Args#connection_params.port, Args#connection_params.db, Args#connection_params.password),
    Redis.

get_structures_names() ->
    {ok, QueueKey} = application:get_env(queue_key),
    {ok, ResultSetKey} = application:get_env(result_set_key),
    RedisData = #structures_names{
        queue_key = QueueKey,
        result_set_key = ResultSetKey},
    RedisData.
