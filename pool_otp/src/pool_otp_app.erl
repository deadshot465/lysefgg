%%%-------------------------------------------------------------------
%% @doc pool_otp public API
%% @end
%%%-------------------------------------------------------------------

-module(pool_otp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pool_otp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
