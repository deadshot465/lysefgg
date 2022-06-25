%%%-------------------------------------------------------------------
%%% @author deadshot465
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 16:21
%%%-------------------------------------------------------------------
-module(counter1).
-author("deadshot465").

%% API
-export([start/0, tick/0, tick/1, read/0, handle/2]).

start() ->
  gen_server_lite:start(?MODULE, 0).

tick() -> tick(1).

tick(Count) -> gen_server_lite:rpc(?MODULE, {tick, Count}).

read() -> gen_server_lite:rpc(?MODULE, read).

handle({tick, Count}, State) ->
  {reply, ack, State + Count};
handle(read, State) ->
  {reply, State, State}.