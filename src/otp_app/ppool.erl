%%%-------------------------------------------------------------------
%%% @author deadshot465
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Sep 2022 18:06
%%%-------------------------------------------------------------------
-module(ppool).
-author("deadshot465").

%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1, run/2, sync_queue/2, async_queue/2]).

start_link() -> ppool_supersup:start_link().

stop() -> ppool_supersup:stop().

start_pool(Name, Limit, {M, F, A}) -> ppool_supersup:start_pool(Name, Limit, {M, F, A}).

stop_pool(Name) -> ppool_supersup:stop_pool(Name).

run(Name, Args) -> ppool_serv:run(Name, Args).

sync_queue(Name, Args) -> ppool_serv:sync_queue(Name, Args).

async_queue(Name, Args) -> ppool_serv:async_queue(Name, Args).