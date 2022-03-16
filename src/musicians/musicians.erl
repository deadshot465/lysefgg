-module(musicians).
-behaviour(gen_server).
-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {name = "", role, skill = good}).
-define(DELAY, 750).

-spec start_link(atom(), _) -> 'ignore' | {'error', _} | {'ok', pid() | {pid(), reference()}}.
start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) -> gen_server:call(Role, stop).

init([Role, Skill]) ->
    %% To know when the parent shuts down
    process_flag(trap_exit, true),
    %% sets a seed for random number generation for the life of the process
    %% uses the current time to do it. Unique value guaranteed by now()
    rand:seed('default', erlang:system_time(millisecond)),
    TimeToPlay = rand:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),
    io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
    {ok, #state{name = Name, role = StrRole, skill = Skill}, TimeToPlay}.

-spec handle_call(_, _, _) -> {'noreply', _, 750} | {'stop', 'normal', 'ok', #state{}}.
handle_call(stop, _From, State = #state{}) ->
    {stop, normal, ok, State};

handle_call(_Message, _From, State) ->
    {noreply, State, ?DELAY}.

-spec handle_cast(_, _) -> {'noreply', _, 750}.
handle_cast(_Message, State) ->
    {noreply, State, ?DELAY}.

-spec handle_info(_, _) -> {'noreply', _, 750} | {'stop', 'bad_note', #state{skill::'bad'}}.
handle_info(timeout, State = #state{name = Name, skill = good}) ->
    io:format("~s produced sound!~n", [Name]),
    {noreply, State, ?DELAY};
handle_info(timeout, State = #state{name = Name, skill = bad}) ->
    case rand:uniform(5) of
        1 ->
            io:format("~s played a false note. Uh oh~n", [Name]),
            {stop, bad_note, State};
        _ ->
            io:format("~s produced sound!~n", [Name]),
            {noreply, State, ?DELAY}
    end;
handle_info(_Message, State) ->
    {noreply, State, ?DELAY}.

-spec code_change(_, _, _) -> {'ok', _}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(_, #state{}) -> 'ok'.
terminate(normal, State) ->
    io:format("~s left the room (~s)~n", [State#state.name, State#state.role]);
terminate(bad_note, State) ->
    io:format("~s sucks! kicked that member out of the band! (~s)~n", [State#state.name, State#state.role]);
terminate(shutdown, State) ->
    io:format("The manager is mad and fired the whole band!"
              "~s just got back to playing in the subway~n", [State#state.name]);
terminate(_Reason, State) ->
    io:format("~s has been kicked out (~s)~n", [State#state.name, State#state.role]).

pick_name() ->
    %% the seed must be set for the random functions. Use within the
    %% process that started with init/1
    rand:seed('default', erlang:system_time(millisecond)),
    lists:nth(rand:uniform(13), first_names())
    ++ " " ++
    lists:nth(rand:uniform(13), last_names()).

first_names() ->
    ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
     "Phoebe", "Ralphie", "Tim", "Wanda", "Janet",
     "Leo", "Yuhei", "Carson", "Miko"].

last_names() ->
    ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
     "Terese", "Tennelli", "Jamal", "Li", "Perlstein",
     "Fujioka", "Ito", "Hage", "Choi"].