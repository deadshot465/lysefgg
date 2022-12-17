%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(erlcount).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, _StartArgs) ->
  erlcount_sup:start_link().

stop(_State) ->
  ok.
