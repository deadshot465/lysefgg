-module(musicians).
-behaviour(gen_server).
-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

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

handle_call(stop, _From, State = #state{}) ->
    {stop, normal, ok, State};

handle_call(_Message, _From, State) ->
    {noreply, State, ?DELAY}.

handle_cast(_Message, State) ->
    {noreply, State, ?DELAY}.

pick_name() ->
    %% the seed must be set for the random functions. Use within the
    %% process that started with init/1
    lists:nth(rand:uniform(13), first_names())
    ++ " " ++
    lists:nth(rand:uniform(13), last_names()).

first_names() ->
    ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
     "Phoebe", "Ralphie", "Tim", "Wanda", "Janet",
     "Leo", "Yuhei", "Carson"].

last_names() ->
    ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
     "Terese", "Tennelli", "Jamal", "Li", "Perlstein",
     "Fujioka", "Ito", "Hage"].