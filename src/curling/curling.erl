-module(curling).
-export([start_link/2, set_teams/3, add_points/3, next_round/1, join_feed/2, leave_feed/2, game_info/1]).

-spec start_link(_, _) -> {'ok', pid()}.
start_link(TeamA, TeamB) ->
    {ok, Pid} = gen_event:start_link(),
    %% The scoreboard will always be there
    gen_event:add_handler(Pid, curling_scoreboard, []),
    gen_event:add_handler(Pid, curling_accumulator, []),
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

join_feed(Pid, ToPid) ->
    HandlerId = {curling_feed, make_ref()},
    gen_event:add_handler(Pid, HandlerId, [ToPid]),
    HandlerId.

leave_feed(Pid, HandlerId) ->
    gen_event:delete_handler(Pid, HandlerId, leave_feed).

game_info(Pid) ->
    gen_event:call(Pid, curling_accumulator, game_data).