-module(curling).
-export([start_link/2, set_teams/3, add_points/3, next_round/1]).

-spec start_link(_, _) -> {'ok', pid()}.
start_link(TeamA, TeamB) ->
    {ok, Pid} = gen_event:start_link(),
    %% The scoreboard will always be there
    gen_event:add_handler(Pid, curling_scoreboard, []),
    set_teams(Pid, TeamA, TeamB),
    {ok, Pid}.

-spec set_teams(atom() | pid() | {atom(), _} | {'via', atom(), _}, _, _) -> 'ok'.
set_teams(Pid, TeamA, TeamB) ->
    gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

-spec add_points(atom() | pid() | {atom(), _} | {'via', atom(), _}, _, _) -> 'ok'.
add_points(Pid, Team, N) ->
    gen_event:notify(Pid, {add_points, Team, N}).

-spec next_round(atom() | pid() | {atom(), _} | {'via', atom(), _}) -> 'ok'.
next_round(Pid) ->
    gen_event:notify(Pid, next_round).