-module(curling_scoreboard).
-behaviour(gen_event).
-export([set_teams/2, next_round/0, add_point/1, reset_board/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-spec set_teams(_, _) -> 'ok'.
set_teams(TeamA, TeamB) ->
    io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

-spec next_round() -> 'ok'.
next_round() ->
    io:format("Scoreboard: Round over~n").

-spec add_point(_) -> 'ok'.
add_point(Team) ->
    io:format("Scoreboard: Increased score of team ~s by 1~n", [Team]).

-spec reset_board() -> 'ok'.
reset_board() ->
    io:format("Scoreboard: All teams are undefined and all scores are 0~n").

-spec init([]) -> {'ok', []}.
init([]) -> {ok, []}.

handle_event({set_teams, TeamA, TeamB}, State) ->
    curling_scoreboard:set_teams(TeamA, TeamB),
    {ok, State};

handle_event({add_points, Team, N}, State) ->
    [curling_scoreboard:add_point(Team) || _ <- lists:seq(1, N)],
    {ok, State};

handle_event(next_round, State) ->
    curling_scoreboard:next_round(),
    {ok, State};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.