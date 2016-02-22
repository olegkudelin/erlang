-module(storage_server_my_2).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(params, {
    redis,
    queue_key,
    result_set_key
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/1, stop/0, get_from_list/0, put_in_set/1, clear_list/0, put_in_list/1, get_and_remove_range_from_list/0, put_in_list1/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(MaxNumber) ->
    gen_server:call(?MODULE, {start, MaxNumber}).
stop() ->
    gen_server:call(?MODULE, stop).

get_from_list() ->
    gen_server:call(?MODULE, get_from_list).

get_and_remove_range_from_list() ->
    gen_server:call(?MODULE, get_and_remove_range_from_list).

put_in_set(Number) ->
    gen_server:cast(?MODULE, {put_in_set, Number}).

clear_list() ->
    gen_server:cast(?MODULE, clear_list).

put_in_list(Number) ->
    gen_server:cast(?MODULE, {put_in_list, Number}).

put_in_list1(Number) ->
    gen_server:cast(?MODULE, {put_in_list1, Number}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, QueueKey} = application:get_env(queue_key),
    {ok, ResultSetKey} = application:get_env(result_set_key),
    RedisParams = #params{redis = redis_connection_pull:get(),
        queue_key = QueueKey,
        result_set_key = ResultSetKey},
    {ok, RedisParams}.

handle_call(get_from_list, _From, State) ->
    {reply, eredis:q(State#params.redis, ["RPOP", State#params.queue_key]), State};

handle_call(get_and_remove_range_from_list, _From, State) ->
    {ok, Data} = eredis:q(State#params.redis, ["LRANGE", State#params.queue_key, 0, 1000]),
    DataLength = erlang:length(Data),
    if
        DataLength > 0 ->
            {ok,<<"OK">>} = eredis:q(State#params.redis, ["LTRIM", State#params.queue_key, DataLength, -1]);
        true -> ok
    end,
    {reply, {ok, Data}, State}.

handle_cast({put_in_set, Number}, State) ->
    eredis:q(State#params.redis, ["SADD", State#params.result_set_key, Number]),
    {noreply, State};

handle_cast({put_in_list, Number}, State) ->
    eredis:q(State#params.redis, ["RPUSH", State#params.queue_key, Number]),
    {noreply, State};

handle_cast({put_in_list1, Number}, State) ->
    eredis:q(State#params.redis, ["RPUSH", "fffff", Number]),
    {noreply, State};

handle_cast({clear_list, Number}, State) ->
    eredis:q(State#params.redis, ["DEL", State#params.queue_key]),
    {noreply, State};

handle_cast(continue, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
