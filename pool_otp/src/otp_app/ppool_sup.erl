-module(ppool_sup).
-export([start_link/3, init/1]).
-behaviour(supervisor).

-spec start_link(_, _, _) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Name, Limit, MFA) ->
  supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
  MaxRestart = 1,
  MaxTime = 3600,
  SupFlags = #{
    strategy => one_for_all,
    intensity => MaxRestart,
    period => MaxTime,
    auto_shutdown => never
  },
  ChildSpecs = [
    #{
      id => serv,
      start => {ppool_serv, start_link, [Name, Limit, self(), MFA]},
      restart => permanent,
      significant => false,
      shutdown => 5000,
      type => worker,
      modules => [ppool_serv]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.