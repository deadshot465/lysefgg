-module(dumb_lib).
-behaviour(gen_server).
-export([open/0, borrow_book/2, return_book/2, close/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2]).

-spec open() -> 'ignore' | {'error', _} | {'ok', pid() | {pid(), reference()}}.
open() ->
    start_link().

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid() | {pid(), reference()}}.
start_link() ->
    gen_server:start_link(?MODULE, ["Harry Potter", "Narnia", "Maelstrom", "Erlang Programming"], []).

borrow_book(Pid, Name) ->
    gen_server:call(Pid, {borrow, Name}).

return_book(Pid, Name) ->
    gen_server:cast(Pid, {return, Name}).

close(Pid) ->
    gen_server:call(Pid, terminate).

%% Internal functions
init(Args) -> {ok, Args}.

handle_call({borrow, Name}, _From, State) ->
    if State =:= [] ->
        {reply, "There are no books anymore!", State};
       State =/= [] ->
        Result = lists:filter(fun(Book) -> string:equal(Book, Name) end, State),
        if length(Result) =/= 0 ->
            Rest = lists:filter(fun(Book) -> Book =/= Name end, State),
            {reply, hd(Result), Rest};
           true -> {reply, "Cannot find your book!", State}
        end
    end;

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({return, Name}, State) ->
    {noreply, [Name | State]}.

handle_info(Message, State) ->
    io:format("Unexpected message: ~p~n", [Message]),
    {noreply, State}.

terminate(normal, State) ->
    [io:format("~p was burnt.~n", [Name]) || Name <- State],
    ok.