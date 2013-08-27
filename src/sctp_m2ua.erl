% M2UA in accordance with RFC3331 (http://tools.ietf.org/html/rfc3331)

% (C) 2011-2013 by Harald Welte <laforge@gnumonks.org>
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

-module(sctp_m2ua).
-author('Harald Welte <laforge@gnumonks.org>').
-behaviour(sctp_core).

-include_lib("kernel/include/inet_sctp.hrl").
-include("osmo_util.hrl").
-include("xua.hrl").
-include("m2ua.hrl").
-include("mtp3.hrl").

-define(M2UA_STREAM_USER,	1).

-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3]).

-export([rx_sctp/4, mtp_xfer/2, state_change/3, prim_up/3]).

-record(m2ua_state, {
		asp_pid,
		last_bsn_received,
		last_fsn_sent,
		role
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_fsm callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Role]) ->
	Fun = fun(Prim, Args) -> asp_prim_to_user(Prim, Args) end,
	AsPid = undefined, % FIXME
	% we use sua_asp module, as m2ua has no difference here
	{ok, Asp} = gen_fsm:start_link(xua_asp_fsm, [AsPid, sua_asp, [], Fun, [self()], self(), Role], [{debug, [trace]}]),
	{ok, #m2ua_state{last_bsn_received=16#ffffff,
			last_fsn_sent=16#ffffff, asp_pid=Asp, role=Role}}.

terminate(Reason, _State, _LoopDat) ->
	io:format("Terminating ~p (Reason ~p)~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, _State, LoopDat, _Extra) ->
	{ok, LoopDat}.

handle_event(_Event, State, LoopDat) ->
	{next_state, State, LoopDat}.

handle_info(_Info, State, LoopDat) ->
	{next_state, State, LoopDat}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sctp_core callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_up(#primitive{subsystem='M', gen_name = 'SCTP_ESTABLISH', spec_name = confirm}, State, LoopDat) ->
	% confirmation in case of active/connect mode
	Asp = LoopDat#m2ua_state.asp_pid,
	gen_fsm:send_event(Asp, osmo_util:make_prim('M','ASP_UP',request)),
	{ignore, LoopDat};
prim_up(#primitive{subsystem='M', gen_name = 'SCTP_ESTABLISH', spec_name = indication}, State, LoopDat) ->
	% indication in case of passive/listen mode
	{ignore, LoopDat};
prim_up(#primitive{subsystem='M', gen_name = 'ASP_UP', spec_name = confirm}, State, LoopDat) ->
	% confirmation in case of active/connect mode
	Asp = LoopDat#m2ua_state.asp_pid,
	% override mode, interface ID  1. FIXME: user-specify interface ID(s)
	Pars = [{?M2UA_P_COM_TRAF_MODE_T, {4, 1}}, {?M2UA_P_COM_INTF_ID_INT, {4, 0}}],
	gen_fsm:send_event(Asp, osmo_util:make_prim('M','ASP_ACTIVE',request,Pars)),
	{ignore, LoopDat};
prim_up(#primitive{subsystem='M', gen_name = 'ASP_ACTIVE', spec_name = confirm}, State, LoopDat) ->
	% FIXME: start a timer or have a separate FSM about it!
	M = #xua_msg{version = 1, msg_class = ?M2UA_MSGC_MAUP,
		     msg_type = ?M2UA_MAUP_MSGT_EST_REQ,
		     payload = [{?M2UA_P_COM_INTF_ID_INT, {4, 0}}]},
	LoopDat2 = mtp_xfer(M, LoopDat),
	{ignore, LoopDat2};



prim_up(Prim, State, LoopDat) ->
	% default: forward all primitives to the user
	{ok, Prim, LoopDat}.


% sctp_core indicates that we have received some data...
rx_sctp(#sctp_sndrcvinfo{ppid = ?M2UA_PPID}, Data, State, LoopDat) ->
	Asp = LoopDat#m2ua_state.asp_pid,
	M2ua = xua_codec:parse_msg(Data),
	% FIXME: check sequenc number linearity
	case M2ua of
		#xua_msg{msg_class = ?M2UA_MSGC_ASPSM} ->
			gen_fsm:send_event(Asp, M2ua),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M2UA_MSGC_ASPTM} ->
			gen_fsm:send_event(Asp, M2ua),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
			 msg_type = ?M2UA_MAUP_MSGT_CONG_IND} ->
			% FIXME
			error_logger:error_report(["unimplemented message",
						   {msg_type, "CONG_IND"}]),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
			 msg_type = ?M2UA_MAUP_MSGT_DATA_RETR_REQ} ->
			% FIXME
			error_logger:error_report(["unimplemented message",
						   {msg_type, "RETR_REQ"}]),
			{ignore, LoopDat};
		#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
			  msg_type = ?M2UA_MAUP_MSGT_DATA} ->
			{_Len, M3bin} = proplists:get_value(?M2UA_P_M2UA_DATA1, M2ua#xua_msg.payload),
			Mtp3 = mtp3_codec:parse_mtp3_msg(M3bin),
			Prim = osmo_util:make_prim('MTP','TRANSFER',indication, Mtp3),
			{ok, Prim, LoopDat};
		_ ->
			rx_sctp(M2ua, State, LoopDat)
	end.

% SG side
rx_sctp(#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
		 msg_type = ?M2UA_MAUP_MSGT_EST_REQ}, State,
	LoopDat = #m2ua_state{role=sg}) ->
	% FIXME: respond with M2UA_MAUP_MSGT_EST_CONF
	error_logger:error_report(["unimplemented message",
				   {msg_type, "EST_REQ"}]),
	{ignore, LoopDat};
rx_sctp(#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
		 msg_type = ?M2UA_MAUP_MSGT_REL_REQ}, State,
	LoopDat = #m2ua_state{role=sg}) ->
	% FIXME: respond with M2UA_MAUP_MSGT_REL_CONF
	error_logger:error_report(["unimplemented message",
				   {msg_type, "REL_REQ"}]),
	{ignore, LoopDat};

rx_sctp(M2ua = #xua_msg{msg_class = ?M2UA_MSGC_MAUP,
		 msg_type = ?M2UA_MAUP_MSGT_STATE_REQ}, State,
        LoopDat = #m2ua_state{role=sg})	->
	handle_m2ua_state_req(M2ua),
	{ignore, LoopDat};

% ASP side
rx_sctp(#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
		 msg_type = ?M2UA_MAUP_MSGT_REL_CONF}, State,
	LoopDat = #m2ua_state{role=asp}) ->
	error_logger:error_report(["unimplemented message",
				   {msg_type, "REL_CONF"}]),
	{ignore, LoopDat};

rx_sctp(#xua_msg{msg_class = ?M2UA_MSGC_MAUP,
		msg_type = ?M2UA_MAUP_MSGT_EST_CONF}, State,
	LoopDat = #m2ua_state{role=asp}) ->
	error_logger:error_report(["unimplemented message",
				   {msg_type, "EST_CONF"}]),
	{ignore, LoopDat};

rx_sctp(M2ua = #xua_msg{}, State, LoopDat) ->
	% do something with link rel msgs
	io:format("M2UA Unknown message ~p in state ~p~n", [M2ua, State]),
	{ignore, LoopDat}.


% MTP-TRANSFER.req has arrived at sctp_core, encapsulate+tx it
mtp_xfer(M2ua, LoopDat) when is_record(M2ua, xua_msg) ->
	M2uaBin = xua_codec:encode_msg(M2ua),
	tx_sctp(?M2UA_STREAM_USER, M2uaBin),
	LoopDat;

mtp_xfer(Mtp3, LoopDat) when is_record(Mtp3, mtp3_msg) ->
	MsgBin = mtp3_codec:encode_mtp3_msg(Mtp3),
	mtp_xfer(MsgBin, LoopDat);

mtp_xfer(Mtp3bin, LoopDat) when is_binary(Mtp3bin) ->
	M2ua = #xua_msg{version = 1,
			 msg_class = ?M2UA_MSGC_MAUP,
			 msg_type = ?M2UA_MAUP_MSGT_DATA,
			 payload = [{?M2UA_P_COM_INTF_ID_INT, {4, 0}},
				    {?M2UA_P_M2UA_DATA1, {byte_size(Mtp3bin), Mtp3bin}}]},
	mtp_xfer(M2ua, LoopDat).

state_change(_, established, LoopDat) ->
	% emulate a 'start' from LSC
	%gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, start),
	LoopDat;
state_change(established, _, LoopDat) ->
	%gen_fsm:send_event(LoopDat#m2pa_state.lsc_pid, link_failure),
	LoopDat;
state_change(_, _, LoopDat) ->
	LoopDat.

handle_m2ua_state_req(M2ua = #xua_msg{payload = Payload}) ->
	{?M2UA_P_MAUP_STATE, State} = lists:keyfind(?M2UA_P_MAUP_STATE, 1, Payload),
	% FIXME handle_m2ua_state_req(State).
	% LOP_SET/CLEAR, EMER_SET/CLEAR, FLUSH_BUFFERSm CONTINUE, CLEAR_RTB, AUDIT, CONG*
	% FIXME: respond with M2UA_MAUP_MSGT_STATE_CONF
	error_logger:error_report(["unimplemented message",
				   {msg_type, "STATE_REQ"}]),
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tx_sctp(Stream, Payload) when is_integer(Stream), is_binary(Payload) ->
	Param = {Stream, ?M2UA_PPID, Payload},
	% sent to 'ourselves' (behaviour master module)
	gen_fsm:send_event(self(), osmo_util:make_prim('SCTP','TRANSFER',request,Param)).

% callback fun for ASP FMS
asp_prim_to_user(Prim, [SctpPid]) ->
	gen_fsm:send_event(SctpPid, Prim).
