-module(america).
-behaviour(gen_server).
-export([start_link/0, order_food/2, close/1, add_chain/2, remove_chain/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid() | {pid(), reference()}}.
start_link() ->
    gen_server:start_link(?MODULE, ["McDonalds", "Kentucky", "Wendy's"], []).

order_food(Pid, Name) ->
    gen_server:call(Pid, Name).

add_chain(Pid, Name) ->
    gen_server:cast(Pid, {add, Name}).

remove_chain(Pid, Name) ->
    gen_server:cast(Pid, {remove, Name}).

close(Pid) ->
    gen_server:call(Pid, terminate).

-spec init(_) -> {'ok', _}.
init(Args) ->
    %% process_flag(trap_exit, true),
    {ok, Args}.

handle_call(mc_donalds, _From, State) ->
    case lists:any(fun(Name) -> Name =:= "McDonalds" end, State) of
        true -> {reply, "Fuck yeah!~n", State};
        false -> {reply, "Hell, no.~n", ["McDonalds" | State]}
    end;

handle_call(kentucky, _From, State) ->
    case lists:any(fun(Name) -> Name =:= "Kentucky" end, State) of
        true -> {reply, "Have some real chicken here!~n", State};
        false -> {reply, "Hell, no.~n", ["Kentucky" | State]}
    end;

handle_call(wendys, _From, State) ->
    case lists:any(fun(Name) -> Name =:= "Wendy's" end, State) of
        true -> {reply, "Bruh, we have pasta, you losers!~n", State};
        false -> {reply, "Hell, no.~n", ["Wendy's" | State]}
    end;

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call(_, _, State) ->
    io:format("What r ya gonna order anyway?~n", []),
    {noreply, State}.

handle_cast({add, Name}, State) ->
    Exists = lists:member(Name, State),
    if Exists =:= true ->
        io:format("We already have it!~n", []),
        {noreply, State};
       true ->
        {noreply, [Name | State]}
    end;

handle_cast({remove, Name}, State) ->
    Exists = lists:member(Name, State),
    if Exists =:= false ->
        io:format("How are we supposed to remove something that doesn't exist in our great America?!~n", []),
        {noreply, State};
       true ->
        {noreply, State -- [Name]}
    end;

handle_cast(_, State) ->
    io:format("Not gonna happen.~n", []),
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, State}.

terminate(normal, State) ->
    lists:foreach(fun(Name) -> io:format("~s has been forced to suspend operation due to COVID-19.~n", [Name]) end, State).