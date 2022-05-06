-module(ppool_supersup).
-behaviour(supervisor).
-export([start_link/0, init/1, stop/0, start_pool/3, stop_pool/1]).

-spec start_pool(_, _, _) -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start_pool(Name, Limit, MFA) ->
    ChildSpec = #{
        id => Name,
        start => {ppool_sup, start_link, [Name, Limit, MFA]},
        restart => permanent,
        significant => false,
        shutdown => 10500,
        type => supervisor,
        modules => [ppool_sup]
    },
    supervisor:start_child(ppool, ChildSpec).

-spec stop_pool(_) -> 'ok' | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

-spec stop() -> 'ok' | 'true'.
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestart,
        period => MaxTime,
        auto_shutdown => never
    },
    {ok, {SupFlags, []}}.