%%%-------------------------------------------------------------------
%%% @author deadshot465
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2022 15:59
%%%-------------------------------------------------------------------
-module(gen_server_lite).
-author("deadshot465").

%% API
-export([start/2, init/2, rpc/2]).

start(Module, State) ->
  register(Module, spawn(?MODULE, init, [Module, State])).

init(Module, State) ->
  loop(Module, State).

loop(Module, State) ->
  receive
    {From, Tag, Request} ->
      {reply, Reply, NewState} = Module:handle(Request, State),
      From ! {Reply, Tag},
      loop(Module, NewState)
  end.

rpc(Module, Request) ->
  Tag = make_ref(),
  Module ! {self(), Tag, Request},
  receive
    {Reply, Tag} when is_atom(Reply) ->
      io:format("Received response from server: ~s~n", [atom_to_list(Reply)]);
    {Reply, Tag} when is_integer(Reply) ->
      io:format("Received response from server: ~s~n", [integer_to_list(Reply)])
  end.