% RFC 3868 SUA SCCP Adaption Layer coding / decoding

% (C) 2012 by Harald Welte <laforge@gnumonks.org>
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

-module(xua_codec).
-author('Harald Welte <laforge@gnumonks.org>').
-include("xua.hrl").

-export([parse_msg/1, encode_msg/1, parse_xua_opts/1, encode_xua_opts/1]).

parse_msg(DataBin) when is_binary(DataBin) ->
	<<Version:8, _Reserved:8, MsgClass:8, MsgType:8, MsgLen:32/big, Remain/binary>> = DataBin,
	RemainLen = MsgLen - 4,
	OptList = parse_xua_opts(Remain),
	#xua_msg{version = Version, msg_class = MsgClass, msg_type = MsgType,
		 payload = OptList};
parse_msg(Data) when is_list(Data) ->
	parse_msg(list_to_binary(Data)).

parse_xua_opts(OptBin) when is_binary(OptBin) ->
	parse_xua_opts(OptBin, []).

parse_xua_opts(<<>>, OptList) when is_list(OptList) ->
	OptList;
parse_xua_opts(OptBin, OptList) when is_binary(OptBin), is_list(OptList) ->
	<<Tag:16/big, LengthIncHdr:16/big, Remain/binary>> = OptBin,
	Length = LengthIncHdr - 4,
	PadLength = get_num_pad_bytes(Length),
	%io:format("Tag ~w, LenInHdr ~w, Len ~w, PadLen ~w, Remain ~w(~p)~n",
	%	  [Tag, LengthIncHdr, Length, PadLength, byte_size(Remain), Remain]),
	<<Value:Length/binary, PadNextOpts/binary>> = Remain,
	% this is ridiculous, we cannot use "<<Value:Length/binary,
	% 0:PadLength, Remain/binary>>" as the last part would not match an
	% empty binary <<>> anymore.  Without the "0:PadLengh" this works
	% perfectly fine.  Now we need some complicated construct and check if
	% the resulting list would be empty :((
	if
		byte_size(PadNextOpts) > PadLength ->
			<<0:PadLength/integer-unit:8, NextOpts/binary>> = PadNextOpts;
		true ->
			NextOpts = <<>>
	end,
	NewOpt = {Tag, {Length, Value}},
	parse_xua_opts(NextOpts, OptList ++ [NewOpt]).



encode_msg(#xua_msg{version = Version, msg_class = MsgClass,
		    msg_type = MsgType, payload = OptList}) ->
	OptBin = encode_xua_opts(OptList),
	MsgLen = byte_size(OptBin) + 8,
	<<Version:8, 0:8, MsgClass:8, MsgType:8, MsgLen:32/big, OptBin/binary>>.

encode_xua_opts(OptList) when is_list(OptList) ->
	encode_xua_opts(OptList, <<>>).

encode_xua_opts([], Bin) ->
	Bin;
encode_xua_opts([{Iei, Attr}|Tail], Bin) ->
	OptBin = encode_xua_opt(Iei, Attr),
	encode_xua_opts(Tail, <<Bin/binary, OptBin/binary>>).

% convert integer parameters to binary before continuing
encode_xua_opt(Iei, {LenIn, Data}) when is_integer(Iei), is_integer(Data) ->
	encode_xua_opt(Iei, {LenIn, <<Data:LenIn/big-integer-unit:8>>});

encode_xua_opt(Iei, {LenIn, Data}) when is_integer(Iei), is_binary(Data) ->
	Length = LenIn + 4,
	PadLen = get_num_pad_bytes(Length),
	<<Iei:16/big, Length:16/big, Data:LenIn/binary, 0:PadLen/integer-unit:8>>;
encode_xua_opt(Iei, Data) when is_integer(Iei), is_binary(Data) ->
	Length = byte_size(Data) + 4,
	PadLen = get_num_pad_bytes(Length),
	<<Iei:16/big, Length:16/big, Data/binary, 0:PadLen/integer-unit:8>>.

% compute the number of pad bits required after a binary parameter
get_num_pad_bytes(BinLenBytes) ->
	case BinLenBytes rem 4 of
		0 ->    0;
		Val ->  4 - Val
	end.
