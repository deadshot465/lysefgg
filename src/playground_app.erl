%%%-------------------------------------------------------------------
%% @doc playground public API
%% @end
%%%-------------------------------------------------------------------

-module(playground_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    playground_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
