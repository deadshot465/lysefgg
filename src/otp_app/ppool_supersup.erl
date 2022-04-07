-module(ppool_supersup).
-behaviour(supervisor).
-export([start_link/0, init/1, stop/0, start_pool/3, stop_pool/1]).

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

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).

start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

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