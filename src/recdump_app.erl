%% ============================================================================
%%  recdump application module
%% ============================================================================

-module(recdump_app).
-behavior(application).
-export ([start/2, stop/1]).

start(_Type, StartArgs) ->
	recdump_sup:start_link (StartArgs). 

stop(_State) ->
	ok.
