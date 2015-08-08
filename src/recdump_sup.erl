%% ============================================================================
%%  Main supervisor
%% ============================================================================

-module(recdump_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link (StartArgs) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init (_StartArgs) ->
    Recdef  = {recdef, {recdump_def, start_link, []}, permanent, 2000, worker, []},
    {ok, {{one_for_one, 1, 1}, [Recdef]}}.

