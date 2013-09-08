-module(osmo_ss7_app).
-behaviour(application).
-author('Harald Welte <laforge@gnumonks.org>').

% application behaviour callbacks
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).

-export([reload_config/0]).

start(normal, StartArgs) ->
	{ok, Pid} = supervisor:start_link({local, osmo_ss7_sup}, osmo_ss7_sup, StartArgs),
	reload_config(),
	{ok, Pid}.


start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

prep_stop(State) ->
	State.

stop(_State) ->
	ok.

config_change(_Changed, _New, _Removed) ->
	ok.



reload_config() ->
	osmo_util:reload_config(),
	% fixme: why not in config/change/3 ?
	ss7_links:reload_config(),
	ss7_routes:reload_config(),
	ok.
