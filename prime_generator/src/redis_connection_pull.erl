-module(redis_connection_pull).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


-record(redis_connection, {
    host,
    port,
    db,
    password
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get/0]).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, RedisConnectionTuple, []).

get() ->
    gen_server:call(?MODULE, get).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, build_redis_list(Args, [], 5)}.

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
build_redis_list(RedisConnection, List, ElementCount) when ElementCount > 0 ->
    CreateRedisConnection = create_redis_connection(RedisConnection),
    build_redis_list(RedisConnection, [CreateRedisConnection | List], ElementCount - 1).

create_redis_connection(Args) ->
    {ok, Redis} = eredis:start_link(Args#redis_connection.host, Args#redis_connection.port, Args#redis_connection.db, Args#redis_connection.password),
    Redis.
