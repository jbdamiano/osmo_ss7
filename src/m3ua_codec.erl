% M3UA in accordance with RFC4666 (http://tools.ietf.org/html/rfc4666)

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

-module(m3ua_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("m3ua.hrl").
-include("mtp3.hrl").

-export([parse_m3ua_msg/1, encode_m3ua_msg/1]).

%-compile(export_all).

-compile({parse_transform, exprecs}).
-export_records([m3ua_msg]).

% compute the number of pad bits required after a binary parameter
get_num_pad_bytes(BinLenBytes) ->
	case BinLenBytes rem 4 of
		0 ->    0;
		Val ->  4 - Val
	end.

parse_m3ua_msg(DataBin) when is_binary(DataBin) ->
	<<Version:8, _Reserved:8, MsgClass:8, MsgType:8, MsgLen:32/big, Remain/binary>> = DataBin,
	OptList = parse_m3ua_opts(Remain),
	#m3ua_msg{version = Version, msg_class = MsgClass, msg_type = MsgType,
		  msg_length = MsgLen-4, payload = OptList};
parse_m3ua_msg(Data) when is_list(Data) ->
	parse_m3ua_msg(list_to_binary(Data)).

parse_m3ua_opts(OptBin) when is_binary(OptBin) ->
	parse_m3ua_opts(OptBin, []).

parse_m3ua_opts(<<>>, OptList) when is_list(OptList) ->
	OptList;
parse_m3ua_opts(OptBin, OptList) when is_binary(OptBin), is_list(OptList) ->
	<<Tag:16/big, Length:16/big, Remain/binary>> = OptBin,
	PadLen = get_num_pad_bytes(Length),
	LengthNet = Length - 4,
	<<CurOpt:LengthNet/binary, 0:PadLen/integer-unit:8, Remain2/binary>> = Remain,
	NewOpt = parse_m3ua_opt(Tag, CurOpt),
	parse_m3ua_opts(Remain2, OptList ++ [NewOpt]).

parse_m3ua_opt(Opt = ?M3UA_IEI_PROTOCOL_DATA, MsgBin) when is_binary(MsgBin) ->
	<<Opc:32/big, Dpc:32/big, Si:8, Ni:8, Mp:8, Sls:8, Payload/binary>> = MsgBin,
	% The idea is to hand back a #mtp3_msg{} to make upper layers beyond
	% MTP-TRANSFR.{ind,req} unaware of a MTP3 or M3UA lower layer
	{Opt, #mtp3_msg{network_ind = Ni, service_ind = Si,
			routing_label = #mtp3_routing_label{sig_link_sel = Sls,
							    origin_pc = Opc,
							    dest_pc = Dpc},
			payload = Payload, m3ua_mp = Mp}};
parse_m3ua_opt(Opt, Msg) ->
	{Opt, Msg}.



encode_m3ua_msg(#m3ua_msg{version = Version, msg_class = MsgClass,
			  msg_type = ?M3UA_MSGT_ASPSM_ASPUP, payload = OptList}) ->
	OptBin = encode_m3ua_opts(OptList),
    MsgLen = byte_size(OptBin) + 8,
	<<Version:8, 0:8, MsgClass:8, ?M3UA_MSGT_ASPSM_ASPUP:8, MsgLen:32/big, OptBin/binary>>;
encode_m3ua_msg(#m3ua_msg{version = Version, msg_class = MsgClass,
			  msg_type = MsgType, payload = OptList}) ->
	OptBin = encode_m3ua_opts(OptList),
    MsgLen = byte_size(OptBin) + 8,
	<<Version:8, 0:8, MsgClass:8, MsgType:8, MsgLen:32/big, OptBin/binary>>.


encode_m3ua_opts(OptList) when is_list(OptList) ->
	encode_m3ua_opts(OptList, <<>>).

encode_m3ua_opts([], Bin) ->
	Bin;
encode_m3ua_opts([{Iei, Attr}|Tail], Bin) ->
	OptBin = encode_m3ua_opt(Iei, Attr),
	encode_m3ua_opts(Tail, <<Bin/binary, OptBin/binary>>).

encode_m3ua_opt(?M3UA_IEI_PROTOCOL_DATA, Mtp3) when is_record(Mtp3, mtp3_msg) ->
	#mtp3_msg{network_ind = Ni, service_ind = Si,
		  routing_label = #mtp3_routing_label{sig_link_sel = Sls,
						      origin_pc = OpcIn,
						      dest_pc = DpcIn},
		  payload = Payload, m3ua_mp = Mp} = Mtp3,
	Opc = osmo_util:pointcode2int(OpcIn),
	Dpc = osmo_util:pointcode2int(DpcIn),
	case Mp of
		undefined -> MpD = 0;
		_ -> MpD = Mp
	end,
	PayBin = <<Opc:32/big, Dpc:32/big, Si:8, Ni:8, MpD:8, Sls:8, Payload/binary>>,
	encode_m3ua_opt(?M3UA_IEI_PROTOCOL_DATA, PayBin);
encode_m3ua_opt(Iei, Data) when is_integer(Iei), is_binary(Data), Data == <<>> ->
	<<>>;
encode_m3ua_opt(Iei, Data) when is_integer(Iei), is_binary(Data) ->
	Length = byte_size(Data) + 4,
	PadLen = get_num_pad_bytes(Length),
	<<Iei:16/big, Length:16/big, Data/binary, 0:PadLen/integer-unit:8>>.
