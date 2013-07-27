% OTP Supervisor for Osmocom SCCP

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
% Additional Permission under GNU AGPL version 3 section 7:
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(osmo_ss7_sup).
-behavior(supervisor).

-export([start_link/0, add_mtp_link/1]).
-export([init/1]).

-include_lib("osmo_ss7/include/osmo_ss7.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [{debug, [trace]}]).

init(Args) ->
	LinksChild = {ss7_links, {ss7_links, start_link, []},
		     permanent, 2000, worker, [ss7_links]},
	RouteChild = {ss7_routes, {ss7_routes, start_link, []},
		     permanent, 2000, worker, [ss7_routes]},
	{ok,{{one_for_one,60,600}, [LinksChild, RouteChild]}}.

% Add a m3ua link to this supervisor
add_mtp_link(L=#sigtran_link{type = m3ua, name = Name,
			   local = Local, remote = Remote}) ->
	ChildName = list_to_atom("ss7_link_m3ua_" ++ Name),
	ChildSpec = {ChildName, {ss7_link_m3ua, start_link, [L]},
		     permanent, 2000, worker, [ss7_link_m3ua]},
	supervisor:start_child(?MODULE, ChildSpec);
add_mtp_link(L=#sigtran_link{type = m2ua, name = Name,
			   local = Local, remote = Remote}) ->
	ChildName = list_to_atom("ss7_link_m2ua_" ++ Name),
	ChildSpec = {ChildName, {ss7_link_m2ua, start_link, [L]},
		     permanent, 2000, worker, [ss7_link_m2ua]},
	supervisor:start_child(?MODULE, ChildSpec);
add_mtp_link(L=#sigtran_link{type = ipa_client, name = Name}) ->
	ChildName = list_to_atom("ss7_link_ipa_client_" ++ Name),
	ChildSpec = {ChildName, {ss7_link_ipa_client, start_link, [L]},
		     permanent, 2000, worker, [ss7_link_ipa_client]},
	supervisor:start_child(?MODULE, ChildSpec);
add_mtp_link([]) ->
	ok;
add_mtp_link([Head|Tail]) ->
	add_mtp_link(Head, Tail).
add_mtp_link(Head, Tail) ->
	{ok, _Child} = add_mtp_link(Head),
	add_mtp_link(Tail).
