% ip.access IPA CCM protocol 

% (C) 2019 by Harald Welte <laforge@gnumonks.org>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

-module(ipa_proto_ccm).
-author('Harald Welte <laforge@gnumonks.org>').

-export([decode/1, encode/1]).

-define(TESTMSG, <<5,0,7,8,48,47,48,47,48,0,0,19,7,48,48,58,48,48,58,48,48,58,48,48,58,48,48,58,
  48,48,0,0,19,2,48,48,58,48,48,58,48,48,58,48,48,58,48,48,58,48,48,0,0,19,3,
  48,48,58,48,48,58,48,48,58,48,48,58,48,48,58,48,48,0,0,19,4,48,48,58,48,48,
  58,48,48,58,48,48,58,48,48,58,48,48,0,0,24,5,111,115,109,111,45,109,115,99,
  45,49,46,51,46,49,46,49,49,45,52,50,55,50,0,0,23,1,77,83,67,45,48,48,45,48,
  48,45,48,48,45,48,48,45,48,48,45,48,48,0,0,23,0,77,83,67,45,48,48,45,48,48,
  45,48,48,45,48,48,45,48,48,45,48,48,0>>).

decode_msgt(0) -> ping;
decode_msgt(1) -> pong;
decode_msgt(4) -> id_req;
decode_msgt(5) -> id_resp;
decode_msgt(6) -> id_ack;
decode_msgt(7) -> id_nack;
decode_msgt(8) -> proxy_req;
decode_msgt(9) -> proxy_ack;
decode_msgt(10) -> proxy_nack;
decode_msgt(Int) when is_integer(Int) -> Int.

encode_msgt(ping) -> 0;
encode_msgt(pong) -> 1;
encode_msgt(id_req) -> 4;
encode_msgt(id_resp) -> 5;
encode_msgt(id_ack) -> 6;
encode_msgt(id_nack) -> 7;
encode_msgt(proxy_req) -> 8;
encode_msgt(proxy_ack) -> 9;
encode_msgt(proxy_nack) -> 10;
encode_msgt(Int) when is_integer(Int) -> Int.

decode_idtag(0) -> serial_nr;
decode_idtag(1) -> unit_name;
decode_idtag(2) -> location;
decode_idtag(3) -> unit_type;
decode_idtag(4) -> equip_vers;
decode_idtag(5) -> sw_version;
decode_idtag(6) -> ip_address;
decode_idtag(7) -> mac_address;
decode_idtag(8) -> unit_id;
decode_idtag(Int) when is_integer(Int) -> Int.

encode_idtag(serial_nr) -> 0;
encode_idtag(unit_name) -> 1;
encode_idtag(location) -> 2;
encode_idtag(unit_type) -> 3;
encode_idtag(equip_vers) -> 4;
encode_idtag(sw_version) -> 5;
encode_idtag(ip_address) -> 6;
encode_idtag(mac_address) -> 7;
encode_idtag(unit_id) -> 8;
encode_idtag(Int) when is_integer(Int) -> Int.

decode(Bin) when is_binary(Bin) ->
	<<MsgType:8, IeList/binary>> = Bin,
	{decode_msgt(MsgType), decode_ies(IeList, [])}.

decode_ies(<<>>, IeList) when is_list(IeList) ->
	IeList;
decode_ies(<<1:8, IdTag:8, Remain/binary>>, IeList) when is_list(IeList) ->
	decode_ies(Remain, IeList ++ [{id, IdTag}]);
decode_ies(<<0:8, Len:8, TypeValue:Len/binary, Remain/binary>>, IeList) when is_list(IeList) ->
	<<Type:8, Value/binary>> = TypeValue,
	ValueList = binary_to_list(Value),
	case lists:last(ValueList) of
		0 -> ValueStripped = lists:droplast(ValueList);
		_ -> ValueStripped = ValueList
	end,
	decode_ies(Remain, IeList ++ [{string, decode_idtag(Type), ValueStripped}]).


encode_ie({id, IdTag}) ->
	<<1:8, IdTag:8>>;
encode_ie({string, Type, Value}) ->
	case lists:last(Value) of
		0 -> ValueTerminated = Value;
		_ -> ValueTerminated = lists:append(Value, [0])
	end,
	ValueBin = list_to_binary(ValueTerminated),
	Type2 = encode_idtag(Type),
	TypeValue = <<Type2:8, ValueBin/binary>>,
	Len = byte_size(TypeValue),
	<<0:8, Len:8, TypeValue/binary>>.

encode_ies(IeList) when is_list(IeList) ->
	encode_ies(IeList, <<>>).
encode_ies([], Bin) -> Bin;
encode_ies([Head|Tail], Bin) ->
	IeBin = encode_ie(Head),
	encode_ies(Tail, <<Bin/binary, IeBin/binary>>).

encode({MsgType, IeList}) ->
	MsgtInt = encode_msgt(MsgType),
	IesBin = encode_ies(IeList),
	<<MsgtInt:8, IesBin/binary>>.
