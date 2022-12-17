%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2022 3:03
%%%-------------------------------------------------------------------
-module(erlcount_dispatch).
-author("chehui.chou").

-behaviour(gen_statem).

%% API
-export([start_link/0, complete/4]).

%% gen_statem callbacks
-export([init/1, format_status/2, dispatching/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(POOL, erlcount).

-record(data, {regex = [], refs = []}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
  gen_statem:start_link(?MODULE, [], []).

-spec complete(atom() | pid(), re:mp(), reference(), non_neg_integer()) -> 'ok'.
complete(Pid, Regex, Ref, Count) ->
  gen_statem:cast(Pid, {complete, Regex, Ref, Count}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  {ok, Re} = application:get_env(regex),
  {ok, Dir} = application:get_env(directory),
  {ok, MaxFiles} = application:get_env(max_files),
  ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
  case lists:all(fun valid_regex/1, Re) of
    true ->
      self() ! {start, Dir},
      {ok, dispatching, #data{regex = [{R, 0} || R <- Re]}};
    false ->
      {stop, invalid_regex}
  end.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
dispatching(info, {start, Dir}, Data) ->
  gen_statem:cast(self(), erlcount_lib:find_erl(Dir)),
  {next_state, dispatching, Data};

dispatching(cast, {continue, File, Continuation}, Data = #data{regex = Re, refs = Refs}) ->
  F = fun({Regex, _Count}, NewRefs) ->
    Ref = make_ref(),
    ppool:async_queue(?POOL, [self(), Ref, File, Regex]),
    [Ref | NewRefs]
      end,
  NewRefs = lists:foldl(F, Refs, Re),
  gen_statem:cast(self(), Continuation()),
  {next_state, dispatching, Data#data{refs = NewRefs}};

dispatching(cast, done, Data) ->
  %% This is a special case. We can not assume that all messages have NOT
  %% been received by the time we hit 'done'. As such, we directly move to
  %% listening/2 without waiting for an external event.
  listening(cast, done, Data);

dispatching(cast, {complete, Regex, Ref, Count}, Data = #data{regex = Re, refs = Refs}) ->
  {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
  NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
  NewData = Data#data{regex = NewRe, refs = Refs -- [Ref]},
  {next_state, dispatching, NewData}.

listening(_, done, #data{regex = Re, refs = []}) ->
  [io:format("Regex ~s has ~p results~n", [R, C]) || {R, C} <- Re],
  {stop, normal, done};

listening(_, done, Data) ->
  {next_state, listening, Data};

listening(cast, {complete, Regex, Ref, Count}, Data = #data{regex = Re, refs = Refs}) ->
  {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
  NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
  NewData = Data#data{regex = NewRe, refs = Refs -- [Ref]},
  listening(cast, done, NewData).

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(EventType, EventContent, State, Data) ->
  io:format("Unexpected event: ~p. Content: ~p.~n", [EventType, EventContent]),
  {next_state, State, Data}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _State, _Data) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

valid_regex(Re) ->
  try re:run("", Re) of
    _ -> true
  catch
    error:badarg -> false
  end.