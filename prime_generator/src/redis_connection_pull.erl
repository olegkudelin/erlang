-module(redis_connection_pull).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


-record(redis_connection, {
    host,
    port,
    db,
    password
}).

-record(redis_data, {
    queue_key,
    result_set_key
}).

-record(state_entity, {
    redis_pid,
    redis_data
}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get/0, get_from_list/1, get_and_remove_range_from_list/1, put_in_set/2, put_in_list/2, put_in_list1/2, clear_list/1]).

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
    RedisConnectionTuple = #redis_connection{
        host = Host,
        port = Port,
        db = DB,
        password = Password
    },
    RedisData = get_redis_data(),
gen_server:start_link({local, ?MODULE}, ?MODULE, {redis_connection, RedisConnectionTuple, redis_data, RedisData}, []).

get() ->
    gen_server:call(?MODULE, get).


get_from_list(RedisConnection) ->
    eredis:q(RedisConnection#state_entity.redis_pid, ["RPOP", RedisConnection#state_entity.redis_data#redis_data.queue_key]).

get_and_remove_range_from_list(RedisConnection) ->
    {ok, Data} = eredis:q(RedisConnection#state_entity.redis_pid, ["LRANGE", RedisConnection#state_entity.redis_data#redis_data.queue_key, 0, 5000]),
    DataLength = erlang:length(Data),
    if
        DataLength > 0 ->
            {ok,<<"OK">>} = eredis:q(RedisConnection#state_entity.redis_pid, ["LTRIM", RedisConnection#state_entity.redis_data#redis_data.queue_key, DataLength, -1]);
        true -> ok
    end,
    {ok, Data}.

put_in_set(Number, RedisConnection) ->
    eredis:q(RedisConnection#state_entity.redis_pid, ["SADD", RedisConnection#state_entity.redis_data#redis_data.result_set_key, Number]).

put_in_list(Number, RedisConnection) ->
    eredis:q(RedisConnection#state_entity.redis_pid, ["RPUSH", RedisConnection#state_entity.redis_data#redis_data.queue_key, Number]).

put_in_list1(Number, RedisConnection) ->
    eredis:q(RedisConnection#state_entity.redis_pid, ["RPUSH", "fffff", Number]).

clear_list(RedisConnection) ->
    eredis:q(RedisConnection#state_entity.redis_pid, ["DEL", RedisConnection#state_entity.redis_data#redis_data.queue_key]).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    RedisList = build_redis_list(Args, [], 5),
    {ok, RedisList}.

handle_call(get, _From, State) ->
    [Redis | RedisList] = State,
    {reply, Redis, lists:append(RedisList, [Redis])}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

build_redis_list(_RedisConnection, List, 0) ->
    List;
build_redis_list(Args, List, ElementCount) when ElementCount > 0 ->
    {redis_connection, RedisConnectionTuple, redis_data, RedisData} = Args,
    CreateRedisConnection = create_redis_connection(RedisConnectionTuple),
    StateEntity = #state_entity{redis_pid = CreateRedisConnection, redis_data = RedisData},
    build_redis_list(Args, [StateEntity | List], ElementCount - 1).

create_redis_connection(Args) ->
    {ok, Redis} = eredis:start_link(Args#redis_connection.host, Args#redis_connection.port, Args#redis_connection.db, Args#redis_connection.password),
    Redis.

get_redis_data() ->
    {ok, QueueKey} = application:get_env(queue_key),
    {ok, ResultSetKey} = application:get_env(result_set_key),
    RedisData = #redis_data{
        queue_key = QueueKey,
        result_set_key = ResultSetKey},
    RedisData.
