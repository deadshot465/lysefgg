-module(ppool_worker_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(MFA = {_, _, _}) ->
    supervisor:start_link(?MODULE, MFA).

init({M, F, A}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestart,
        period => MaxTime,
        auto_shutdown => never
    },
    ChildSpecs = [
        #{
            id => ppool_worker,
            start => {M, F, A},
            restart => temporary,
            significant => false,
            shutdown => 5000,
            type => worker,
            modules => [M]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.