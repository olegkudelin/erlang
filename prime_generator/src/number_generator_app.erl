-module(number_generator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, MaxNumber) ->
    number_generator_sup:start_link(MaxNumber).

stop(_State) ->
    ok.
