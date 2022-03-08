-module(trade_calls).
-compile(export_all).

-spec main_ab() -> pid().
main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> a(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> b(PidA, PidCliA) end).

-spec a(atom() | pid() | port() | reference() | {atom(), atom()}) -> 'ok'.
a(Parent) ->
    {ok, Pid} = tetsu_bank:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid, true)
    timer:sleep(800),
    tetsu_bank:accept_trade(Pid),
    timer:sleep(400),
    io:format("~p~n", [tetsu_bank:ready(Pid)]),
    timer:sleep(1000),
    tetsu_bank:make_offer(Pid, "horse"),
    tetsu_bank:make_offer(Pid, "sword"),
    timer:sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    tetsu_bank:ready(Pid),
    timer:sleep(200),
    tetsu_bank:ready(Pid),
    timer:sleep(1000).

-spec b(_, atom() | pid() | port() | reference() | {atom(), atom()}) -> 'ok'.
b(PidA, PidCliA) ->
    {ok, Pid} = tetsu_bank:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid, true)
    timer:sleep(500),
    tetsu_bank:trade(Pid, PidA),
    tetsu_bank:make_offer(Pid, "boots"),
    timer:sleep(200),
    tetsu_bank:retract_offer(Pid, "boots"),
    timer:sleep(500),
    tetsu_bank:make_offer(Pid, "shotgun"),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliA),
    tetsu_bank:make_offer(Pid, "horse"), %% race condition!
    tetsu_bank:ready(Pid),
    timer:sleep(200),
    timer:sleep(1000).

%% force a race condition on cd trade negotiation
-spec main_cd() -> any().
main_cd() ->
    S = self(),
    PidCliC = spawn(fun() -> c(S) end),
    receive PidC -> PidC end,
    spawn(fun() -> d(S, PidC, PidCliC) end),
    receive PidD -> PidD end,
    PidCliC ! PidD.

-spec c(atom() | pid() | port() | reference() | {atom(), atom()}) -> 'ok'.
c(Parent) ->
    {ok, Pid} = tetsu_bank:start_link("Marc"),
    Parent ! Pid,
    receive PidD -> PidD end,
    io:format("Spawned Marc: ~p~n", [Pid]),
    %sys:trace(Pid, true),
    sync2(),
    tetsu_bank:trade(Pid, PidD),
    %% no need to accept_trade thanks to the race condition
    timer:sleep(200),
    tetsu_bank:retract_offer(Pid, "car"),
    tetsu_bank:make_offer(Pid, "horse"),
    timer:sleep(600),
    tetsu_bank:cancel(Pid),
    timer:sleep(1000).

-spec d(atom() | pid() | port() | reference() | {atom(), atom()}, _, atom() | pid() | port() | reference() | {atom(), atom()}) -> 'ok'.
d(Parent, PidC, PidCliC) ->
    {ok, Pid} = tetsu_bank:start_link("Pete"),
    Parent ! Pid,
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    sync1(PidCliC),
    tetsu_bank:trade(Pid, PidC),
    %% no need to accept_trade thanks to the race condition
    timer:sleep(200),
    tetsu_bank:retract_offer(Pid, "car"),
    tetsu_bank:make_offer(Pid, "manatee"),
    timer:sleep(100),
    tetsu_bank:ready(Pid),
    timer:sleep(1000).

-spec main_ef() -> pid().
main_ef() ->
    S = self(),
    PidCliE = spawn(fun() -> e(S) end),
    receive PidE -> PidE end,
    spawn(fun() -> f(PidE, PidCliE) end).

-spec e(atom() | pid() | port() | reference() | {atom(), atom()}) -> 'ok'.
e(Parent) ->
    {ok, Pid} = tetsu_bank:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    tetsu_bank:accept_trade(Pid),
    timer:sleep(400),
    io:format("~p~n",[tetsu_bank:ready(Pid)]),
    timer:sleep(1000),
    tetsu_bank:make_offer(Pid, "horse"),
    tetsu_bank:make_offer(Pid, "sword"),
    timer:sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    tetsu_bank:ready(Pid),
    timer:sleep(200),
    tetsu_bank:ready(Pid),
    timer:sleep(1000).

-spec f(_, atom() | pid() | port() | reference() | {atom(), atom()}) -> 'ok'.
f(PidE, PidCliE) ->
    {ok, Pid} = tetsu_bank:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    tetsu_bank:trade(Pid, PidE),
    tetsu_bank:make_offer(Pid, "boots"),
    timer:sleep(200),
    tetsu_bank:retract_offer(Pid, "boots"),
    timer:sleep(500),
    tetsu_bank:make_offer(Pid, "shotgun"),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliE),
    tetsu_bank:make_offer(Pid, "horse"),
    timer:sleep(200),
    tetsu_bank:ready(Pid),
    timer:sleep(1000).

%%% Utils
-spec sync1(atom() | pid() | port() | reference() | {atom(), atom()}) -> 'ok'.
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

-spec sync2() -> 'ack'.
sync2() ->
    receive
        From -> From ! ack
    end.