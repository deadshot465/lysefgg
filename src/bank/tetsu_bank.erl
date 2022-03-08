-module(tetsu_bank).
-behaviour(gen_statem).
-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
-export([init/1, callback_mode/0, idle/3, idle_wait/3, negotiate/3, wait/3, ready/3, code_change/4, terminate/3]).
-record(state, {name="", other, own_items=[], other_items=[], monitor, from}).

-spec start(_) -> 'ignore' | {'error', _} | {'ok', pid()}.
start(Name) ->
    gen_statem:start(?MODULE, [Name], []).

-spec start_link(_) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Name) ->
    gen_statem:start_link(?MODULE, [Name], []).

-spec trade(atom() | pid() | {atom(), _} | {'via', atom(), _}, _) -> any().
trade(Pid, OtherPid) ->
    gen_statem:call(Pid, {negotiate, OtherPid}, 30000).

-spec accept_trade(atom() | pid() | {atom(), _} | {'via', atom(), _}) -> any().
accept_trade(Pid) ->
    gen_statem:call(Pid, accept_negotiate).

-spec make_offer(atom() | pid() | {atom(), _} | {'via', atom(), _}, _) -> 'ok'.
make_offer(Pid, Item) ->
    gen_statem:cast(Pid, {make_offer, Item}).

-spec retract_offer(atom() | pid() | {atom(), _} | {'via', atom(), _}, _) -> 'ok'.
retract_offer(Pid, Item) ->
    gen_statem:cast(Pid, {retract_offer, Item}).

-spec ready(atom() | pid() | {atom(), _} | {'via', atom(), _}) -> any().
ready(Pid) ->
    gen_statem:call(Pid, ready, infinity).

-spec cancel(atom() | pid() | {atom(), _} | {'via', atom(), _}) -> 'ok'.
cancel(Pid) ->
    gen_statem:stop(Pid).

%% Internal functions
-spec init(_) -> {'ok', 'idle', #state{own_items::[], other_items::[]}}.
init(Name) ->
    {ok, idle, #state{name=Name}}.

-spec callback_mode() -> 'state_functions'.
callback_mode() -> state_functions.

%% Idle state
-spec idle(_, _, _) -> 'keep_state_and_data' | {'next_state', 'idle_wait', #state{other::atom() | pid() | port() | {atom(), atom()}, monitor::reference()}}.
idle({call, From}, {negotiate, OtherPid}, State=#state{}) ->
    gen_statem:cast(OtherPid, {ask_negotiate, self()}),
    notice(State, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, State#state{other=OtherPid, monitor=Ref, from=From}};

idle(cast, {ask_negotiate, OtherPid}, State=#state{}) ->
    Ref = monitor(process, OtherPid),
    notice(State, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, State#state{other=OtherPid, monitor=Ref}};

idle(_, Event, _) ->
    unexpected(Event, idle),
    keep_state_and_data.

%% Idle Wait state
%% Called when the other side also asks for trade at the same time
-spec idle_wait(_, _, _) -> 'keep_state_and_data' | {'next_state', 'negotiate', #state{from::{pid(), _}}} | {'next_state', 'negotiate', #state{other::atom() | pid() | {atom(), _} | {'via', atom(), _}}, {'reply', _, 'ok'}}.
idle_wait(cast, {ask_negotiate, OtherPid}, State=#state{other=OtherPid}) ->
    gen_statem:reply(State#state.from, ok),
    notice(State, "starting negotiation", []),
    {next_state, negotiate, State};

idle_wait(cast, {accept_negotiate, OtherPid}, State=#state{other=OtherPid}) ->
    gen_statem:reply(State#state.from, ok),
    notice(State, "starting negotiation", []),
    {next_state, negotiate, State};

idle_wait({call, From}, accept_negotiate, State=#state{other=OtherPid}) ->
    gen_statem:cast(OtherPid, {accept_negotiate, self()}),
    notice(State, "accepting negotiation", []),
    {next_state, negotiate, State, {reply, From, ok}};

idle_wait(_, Event, _) ->
    unexpected(Event, idle_wait),
    keep_state_and_data.

%% Negotiate state
-spec negotiate(_, _, _) -> 'keep_state_and_data' | {'keep_state', #state{}} | {'next_state', 'wait', #state{other::atom() | pid() | {atom(), _} | {'via', atom(), _}}}.
negotiate(cast, {make_offer, Item}, State=#state{other=OtherPid, own_items=OwnItems}) ->
    gen_statem:cast(OtherPid, {do_offer, Item}),
    notice(State, "offering ~p", [Item]),
    {keep_state, State#state{own_items=add(Item, OwnItems)}};

negotiate(cast, {retract_offer, Item}, State=#state{other=OtherPid, own_items=OwnItems}) ->
    gen_statem:cast(OtherPid, {undo_offer, Item}),
    notice(State, "cancelling offer on ~p", [Item]),
    {keep_state, State#state{own_items=remove(Item, OwnItems)}};

negotiate(cast, {do_offer, Item}, State=#state{other_items=OtherItems}) ->
    notice(State, "other player offering ~p", [Item]),
    {keep_state, State#state{other_items=add(Item, OtherItems)}};

negotiate(cast, {undo_offer, Item}, State=#state{other_items=OtherItems}) ->
    notice(State, "other player cancelling offer on ~p", [Item]),
    {keep_state, State#state{other_items=remove(Item, OtherItems)}};

negotiate(cast, are_you_ready, State=#state{other=OtherPid}) ->
    io:format("Other user ready to trade.~n"),
    notice(State,
           "Other user ready to transfer goods: ~n"
           "You get ~p, The other side gets ~p",
           [State#state.other_items, State#state.own_items]),
    gen_statem:cast(OtherPid, not_yet),
    keep_state_and_data;

negotiate({call, From}, ready, State=#state{other=OtherPid}) ->
    gen_statem:cast(OtherPid, are_you_ready),
    notice(State, "asking if ready, waiting", []),
    {next_state, wait, State#state{from=From}};

negotiate(EventType, Event, State) ->
    unexpected({EventType, Event, State}, negotiate),
    keep_state_and_data.

%% Wait state
wait(cast, {do_offer, Item}, State=#state{other_items=OtherItems}) ->
    gen_statem:reply(State#state.from, offer_changed),
    notice(State, "other side offering ~p", [Item]),
    {next_state, negotiate, State#state{other_items=add(Item, OtherItems)}};

wait(cast, {undo_offer, Item}, State=#state{other_items=OtherItems}) ->
    gen_statem:reply(State#state.from, offer_changed),
    notice(State, "other side cancelling offer of ~p", [Item]),
    {next_state, negotiate, State#state{other_items=remove(Item, OtherItems)}};

wait(cast, are_you_ready, State=#state{other=OtherPid}) ->
    gen_statem:cast(OtherPid, ready),
    notice(State, "asked if ready, and I am. Waiting for same reply", []),
    keep_state_and_data;

wait(cast, not_yet, State=#state{}) ->
    notice(State, "other side not ready yet", []),
    keep_state_and_data;

wait(cast, ready, State=#state{}) ->
    gen_statem:cast(State#state.other, ready),
    gen_statem:cast(State#state.other, ack),
    gen_statem:reply(State#state.from, ok),
    notice(State, "other side is ready. Moving to ready state", []),
    {next_state, ready, State};

wait(_, Event, _) ->
    unexpected(Event, wait),
    keep_state_and_data.

ready(cast, ack, State=#state{other=OtherPid}) ->
    case priority(self(), OtherPid) of
        true ->
            try
                notice(State, "asking for commit", []),
                ready_commit = gen_statem:call(OtherPid, ask_commit),
                notice(State, "ordering commit", []),
                ok = gen_statem:call(OtherPid, do_commit),
                notice(State, "committing...", []),
                commit(State),
                {stop, normal, State}
            catch Class:Reason ->
                %% abort! Either ready_commit or do_commit failed
                notice(State, "commit failed", []),
                {stop, {Class, Reason}, State}
            end;
        false ->
            keep_state_and_data
    end;

ready({call, From}, ask_commit, State) ->
    notice(State, "replying to ask_commit", []),
    {keep_state_and_data, {reply, From, ready_commit}};

ready({call, _From}, do_commit, State) ->
    notice(State, "committing...", []),
    commit(State),
    {stop, normal, State};

ready(_, Event, _) ->
    unexpected(Event, ready),
    keep_state_and_data.

-spec notice(#state{}, string(), [any()]) -> 'ok'.
notice(#state{name=N}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [N | Args]).

-spec unexpected(_, _) -> 'ok'.
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

-spec add(_, _) -> nonempty_maybe_improper_list().
add(Item, Items) -> [Item | Items].

-spec remove(_, [any()]) -> [any()].
remove(Item, Items) -> Items -- [Item].

-spec priority(pid(), pid()) -> boolean().
priority(Pid, OtherPid) when Pid > OtherPid -> true;
priority(Pid, OtherPid) when Pid < OtherPid -> false.

-spec commit(#state{}) -> any().
commit(State=#state{}) ->
    io:format("Transaction completed for ~s. "
              "Items sent are:~n~p,~n received are:~n~p.~n"
              "This operation should have some atomic save "
              "in a database.~n",
              [State#state.name, State#state.own_items, State#state.other_items]).

-spec code_change(_, _, _, _) -> {'ok', _, _}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

-spec terminate(_, _, _) -> 'ok'.
terminate(normal, ready, State=#state{}) ->
    notice(State, "StateM leaving", []);
terminate(_Reason, _StateName, _State) ->
    ok.