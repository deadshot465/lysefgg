%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Dec 2022 5:33
%%%-------------------------------------------------------------------
-module(erlcount_lib).
-author("chehui.chou").
-include_lib("kernel/include/file.hrl").

%% API
-export([find_erl/1, regex_count/2]).

%% Finds all files ending in .erl
-spec find_erl(binary() | string() | #file_descriptor{module :: atom()}) -> 'done' | {'continue', string() | binary(), fun(() -> 'done' | {_, _, _})}.
find_erl(Directory) ->
  find_erl(Directory, queue:new()).

-spec regex_count(re:mp(), binary() | string() | unicode:charlist()) -> non_neg_integer().
regex_count(Re, Str) ->
  case re:run(Str, Re, [global]) of
    nomatch -> 0;
    {match, List} -> length(List)
  end.

%%% Private
%% Dispatches based on file type
-spec find_erl(string() | binary() | #file_descriptor{module :: atom()}, queue:queue(_)) -> 'done' | {'continue', string() | binary(), fun(() -> 'done' | {_, _, _})}.
find_erl(Name, Queue) ->
  case file:read_file_info(Name) of
    {ok, F = #file_info{}} ->
      case F#file_info.type of
        directory -> handle_directory(Name, Queue);
        regular -> handle_regular_file(Name, Queue);
        _Other -> dequeue_and_run(Queue)
      end;

    {error, _} -> dequeue_and_run(Queue)
  end.

%% Opens directories and enqueues files in there
-spec handle_directory(string() | binary(), queue:queue(_)) -> 'done' | {'continue', string() | binary(), fun(() -> 'done' | {_, _, _})}.
handle_directory(Directory, Queue) ->
  case file:list_dir(Directory) of
    {ok, []} -> dequeue_and_run(Queue);
    {ok, Files} ->
      dequeue_and_run(enqueue_many(Directory, Files, Queue))
  end.

%% Pops an item from the queue and runs it
-spec dequeue_and_run(queue:queue(_)) -> 'done' | {'continue', string() | binary(), fun(() -> 'done' | {_, _, _})}.
dequeue_and_run(Queue) ->
  case queue:out(Queue) of
    {empty, _} -> done;
    {{value, File}, NewQueue} ->
      find_erl(File, NewQueue)
  end.

%% Adds a bunch of items to the queue
-spec enqueue_many(string() | binary(), [string() | binary(), ...], queue:queue(_)) -> queue:queue(_).
enqueue_many(Directory, Files, Queue) ->
  F = fun(File, Q) -> queue:in(filename:join(Directory, File), Q) end,
  lists:foldl(F, Queue, Files).

%% Checks if the file finishes in .erl
-spec handle_regular_file(binary() | string(), queue:queue(_)) -> 'done' | {'continue', string() | binary(), fun(() -> 'done' | {_, _, _})}.
handle_regular_file(Name, Queue) ->
  case filename:extension(Name) of
    ".erl" -> {continue, Name, fun() -> dequeue_and_run(Queue) end};
    _NonErl -> dequeue_and_run(Queue)
  end.