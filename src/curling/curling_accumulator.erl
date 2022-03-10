-module(curling_accumulator).
-behaviour(gen_event).
-record(state, {teams = orddict:new(), round = 0}).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init([]) ->
    {ok, #state{}}.

-spec handle_event(_, _) -> {'ok', _}.
handle_event({set_teams, TeamA, TeamB}, State=#state{teams=T}) ->
    Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
    {ok, State#state{teams=Teams}};
handle_event({add_points, Team, N}, State=#state{teams=T}) ->
    Teams = orddict:update_counter(Team, N, T),
    {ok, State#state{teams=Teams}};
handle_event(next_round, State=#state{}) ->
    {ok, State#state{round=State#state.round + 1}};
handle_event(_Event, Pid) ->
    {ok, Pid}.

-spec handle_call(_, _) -> {'ok', 'ok' | {[{_, _}], {'round', _}}, _}.
handle_call(game_data, State=#state{teams=T, round=R}) ->
    {ok, {orddict:to_list(T), {round, R}}, State};
handle_call(_, State) ->
    {ok, ok, State}.

-spec handle_info(_, _) -> {'ok', _}.
handle_info(_, State) ->
    {ok, State}.

-spec code_change(_, _, _) -> {'ok', _}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(_, _) -> 'ok'.
terminate(_Reason, _State) ->
    ok.