%%%-------------------------------------------------------------------
%%% @author deadshot465
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 15:39
%%%-------------------------------------------------------------------
-module(counter0).
-author("deadshot465").

%% API
-export([start/0, init/1]).
-export([tick/0, read/0, tick/1]).

start() ->
  register(counter0, spawn(counter0, init, [0])).

init(State) ->
  loop(State).

tick() -> tick(1).

tick(Count) -> rpc({tick, Count}).

read() -> rpc(read).

loop(State) ->
  receive
    {From, Tag, {tick, Ticks}} ->
      From ! {ack, Tag},
      loop(State + Ticks);
    {From, Tag, read} ->
      From ! {State, Tag},
      loop(State)
  end.

rpc(Request) ->
  Tag = make_ref(),
  counter0 ! {self(), Tag, Request},
  receive
    {Reply, Tag} when is_atom(Reply) ->
      io:format("Received response from server: ~s~n", [atom_to_list(Reply)]);
    {Reply, Tag} when is_integer(Reply) ->
      io:format("Received response from server: ~s~n", [integer_to_list(Reply)])
  end.