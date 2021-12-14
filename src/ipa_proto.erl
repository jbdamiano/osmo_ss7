% ip.access IPA multiplex protocol 

% (C) 2010,2012,2019 by Harald Welte <laforge@gnumonks.org>
% (C) 2010 by On-Waves
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

-module(ipa_proto).
-author('Harald Welte <laforge@gnumonks.org>').
-compile(export_all).

-include("ipa.hrl").

-define(TIMEOUT, 1000).
-define(IPA_SOCKOPTS, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]).

-define(IPAC_MSGT_PING,		0).
-define(IPAC_MSGT_PONG, 	1).
-define(IPAC_MSGT_ID_GET, 	4).
-define(IPAC_MSGT_ID_RESP, 	5).
-define(IPAC_MSGT_ID_ACK, 	6).

-define(IPAC_PROTO_OSMO,	238).
-define(IPAC_PROTO_CCM,		254).

-export([register_socket/1, register_stream/3, unregister_stream/2,
	 send/3, connect/3, connect/4, listen_accept_handle/2,
	 start_listen/3, controlling_process/3, register_codec/3]).

-type stream_id() :: integer() | {osmo, integer()}.

-record(ipa_socket, {socket, ipaPid, streamTbl, listenType}).

-record(ipa_codec, {streamId :: stream_id(),
		    encodeFn :: fun(),
		    decodeFn :: fun()
	}).

% register a TCP socket with this IPA protocol implementation
register_socket(Socket) ->
	IpaPid = spawn(?MODULE, init_sock, [Socket, self()]),
	% synchronously wait for init_sock to be done
	receive 
		{ipa_init_sock_done, Socket} ->
			% assign ownership of the socket to the new IPA handler process
			gen_tcp:controlling_process(Socket, IpaPid),
			{ok, IpaPid}
	after 
		?TIMEOUT ->
			{error, timeout}
	end.

% call_sync() preceeded by a Socket -> Pid lookup
call_sync_sock(Socket, Request) ->
	% resolve PID responsible for this socket
	case ets:lookup(ipa_sockets, Socket) of
		[IpaSock] ->
			call_sync(IpaSock#ipa_socket.ipaPid, Request);
		_ ->
			io:format("No Process for Socket ~p~n", [Socket]),
			{error, no_sock_for_pid}
	end.

% a user process wants to register itself for a given Socket/StreamID tuple
register_stream(Socket, StreamID, Pid) ->
	call_sync_sock(Socket, {ipa_reg_stream, Socket, StreamID, Pid}).

register_streams(_S, []) ->
	ok;
register_streams(S, [{StreamID, Pid}|SList]) ->
	ipa_proto:register_stream(S, StreamID, Pid),
	register_streams(S, SList).

% unregister for a given stream
unregister_stream(Socket, StreamID) ->
	call_sync_sock(Socket, {ipa_unreg_stream, Socket, StreamID}).

% change the controlling process for a given {Socket, StreamID}
controlling_process(Socket, StreamID, NewPid) ->
	call_sync_sock(Socket, {ipa_ctrl_proc, Socket, StreamID, NewPid}).

% Set the metadata required for the ipa CCM sub-protocol.
set_ccm_options(Socket, CcmOptions) ->
	call_sync_sock(Socket, {ipa_set_ccm_options, Socket, CcmOptions}).

% unblock the socket from further processing
unblock(Socket) ->
	call_sync_sock(Socket, {ipa_unblock, Socket}).

% server-side handler for unregister_stream()
request({ipa_reg_stream, Socket, StreamID, Pid}, _) ->
	io:format("Registering handler ~p for socket ~p Stream ~p~n", [Pid, Socket, StreamID]),
	[IpaSock] = ets:lookup(ipa_sockets, Socket),
	ets:insert_new(IpaSock#ipa_socket.streamTbl, {{Socket, StreamID}, Pid});
% server-side handler for unregister_stream()
request({ipa_unreg_stream, Socket, StreamID}, _) ->
	io:format("Unregistering handler for Socket ~p Stream ~p~n", [Socket, StreamID]),
	[IpaSock] = ets:lookup(ipa_sockets, Socket),
	ets:delete(IpaSock#ipa_socket.streamTbl, {Socket, StreamID});
% server-side handler for controlling_process()
request({ipa_ctrl_proc, Socket, StreamID, NewPid}, _) ->
	io:format("Changing handler for socket ~p Stream ~p~n", [Socket, StreamID]),
	[IpaSock] = ets:lookup(ipa_sockets, Socket),
	ets:delete(IpaSock#ipa_socket.streamTbl, {Socket, StreamID}),
	ets:insert_new(IpaSock#ipa_socket.streamTbl, {{Socket, StreamID}, NewPid});
% server-side handler for set_ccm_options()
% set ccm protocol metadata options reported with connection setup.
request({ipa_set_ccm_options, Socket, CcmOptions}, _) ->
	io:format("Setting ccm options for socket ~p to ~p~n", [Socket, CcmOptions]),
	{ccm_options, CcmOptions};
% server-side handler for unblock()
request({ipa_unblock, Socket}, CcmOptions) ->
	if
		CcmOptions#ipa_ccm_options.initiate_ack -> send_ccm_id_ack(Socket);
		true -> send_ccm_id_get(Socket)
	end,
	io:format("Unblocking socket ~p~n", [Socket]),
	%[IpaSock] = ets:lookup(ipa_sockets, Socket),
	Ret = inet:setopts(Socket, [{active, once}]),
	io:format("Unblocking socket ~p:~p~n", [Socket, Ret]).

% split an incoming IPA message and split it into Length/StreamID/Payload
split_ipa_msg(DataBin) ->
	% FIXME: This will throw an exception if DataBin doesn't contain all payload
	<<Length:16/big-unsigned-integer, StreamID:8, Payload:Length/binary, Trailer/binary>> = DataBin,
	io:format("Stream ~p, ~p bytes~n", [StreamID, Length]),
	{StreamID, Payload, Trailer}.

% deliver an incoming message to the process that is registered for the socket/stream_id
deliver_rx_ipa_msg(Socket, StreamID, StreamMap, DataBin) ->
	DataDec = try_decode(StreamID, DataBin),
	case ets:lookup(StreamMap, {Socket, StreamID}) of
		[{_,{process_id, Pid}}] ->
			Pid ! {ipa, Socket, StreamID, DataDec};
		[{_,{callback_fn, Fn, Args}}] ->
			Fn(Socket, StreamID, DataDec, Args);
		[] ->
			io:format("No Pid registered for Socket ~p Stream ~p~n", [Socket, StreamID])
	end.

% register a Codec with this IPA protocol implementation
-spec register_codec(stream_id(), fun(), fun()) -> boolean().
register_codec(StreamID, EncodeFn, DecodeFn) ->
	ets:insert(ipa_codecs, #ipa_codec{streamId=StreamID, encodeFn=EncodeFn, decodeFn=DecodeFn}).

-spec try_decode(stream_id(), binary()) -> any().
try_decode(StreamID, Data) ->
	case ets:lookup(ipa_codecs, StreamID) of
		[IpaCodec] ->
			Fun = IpaCodec#ipa_codec.decodeFn,
			Fun(Data);
		[] ->
			Data
	end.

-spec try_encode(stream_id(), any()) -> binary().
try_encode(_StreamID, Data) when is_binary(Data) ->
	Data;
try_encode(StreamID, Data) ->
	case ets:lookup(ipa_codecs, StreamID) of
		[IpaCodec] ->
			Fun = IpaCodec#ipa_codec.encodeFn,
			Fun(Data);
		[] ->
			Data
	end.

% process (split + deliver) an incoming IPA message
process_rx_ipa_msg(_S, _StreamMap, _, <<>>) ->
	ok;
process_rx_ipa_msg(S, StreamMap, CcmOptions, Data) ->
	{StreamID, PayloadBin, Trailer} = split_ipa_msg(Data),
	case StreamID of
		?IPAC_PROTO_CCM ->
			process_rx_ccm_msg(S, StreamID, CcmOptions, PayloadBin);
		?IPAC_PROTO_OSMO ->
			<<ExtStreamID:8, PayloadExt/binary>> = PayloadBin,
			deliver_rx_ipa_msg(S, {osmo, ExtStreamID}, StreamMap, PayloadExt);
		_ ->
			deliver_rx_ipa_msg(S, StreamID, StreamMap, PayloadBin)
	end,
	process_rx_ipa_msg(S, StreamMap, CcmOptions, Trailer).

send_close_signal([]) ->
	ok;
send_close_signal([StreamSpec|Tail]) ->
	io:format("send_close_signal ~p ~p~n", [StreamSpec, Tail]),
	case StreamSpec of
		[{{Socket, StreamID}, {process_id, Pid}}] ->
			Pid ! {ipa_closed, {Socket, StreamID}};
		[{{Socket, StreamID}, {callback_fn, Fn, Args}}] ->
			Fn(Socket, StreamID, ipa_closed, Args)
	end,
	send_close_signal(Tail).

process_tcp_closed(S, StreamMap) ->
	% signal the closed socket to the user
	StreamList = ets:match(StreamMap, '$1'),
	send_close_signal(StreamList),
	% remove the stream map for this socket
	ets:delete(StreamMap),
	% remove any entry regarding 'S' from ipa_sockets
	ets:delete(ipa_sockets, S),
	ok.

% send a binary message through a given Socket / StreamID
send(Socket, {osmo, StreamIdExt}, DataBin) ->
	DataEnc = try_encode({osmo, StreamIdExt}, DataBin),
	send(Socket, ?IPAC_PROTO_OSMO, [StreamIdExt, DataEnc]);
send(Socket, StreamID, DataBin) ->
	DataEnc = try_encode(StreamID, DataBin),
	Size = iolist_size(DataEnc),
	gen_tcp:send(Socket, iolist_to_binary([<<Size:2/big-unsigned-integer-unit:8>>, StreamID, DataEnc])).


call_sync(Pid, Request) ->
	Ref = make_ref(),
	Pid ! {request, {self(), Ref}, Request},
	receive
		{reply, Ref, Reply} -> Reply
	after
		?TIMEOUT -> {error, timeout}
	end.

reply({From, Ref}, Reply) ->
	From ! {reply, Ref, Reply}.


% global module initialization
init() ->
	ipa_sockets = ets:new(ipa_sockets, [named_table, set, public, {keypos, #ipa_socket.socket}]),
	ipa_codecs = ets:new(ipa_codecs, [named_table, set, public, {keypos, #ipa_codec.streamId}]).

% initialize a signle socket, create its handle process
init_sock(Socket, CallingPid) ->
	StreamMap = ets:new(stream_map, [set]),
	ets:insert(ipa_sockets, #ipa_socket{socket=Socket, ipaPid=self(), streamTbl=StreamMap}),
	CallingPid ! {ipa_init_sock_done, Socket},
	loop(Socket, StreamMap, #ipa_ccm_options{}).

loop(S, StreamMap, CcmOptions) ->
	receive
		{request, From, Request} ->
			case ipa_proto:request(Request, CcmOptions) of
				{ccm_options, NewCcmOptions} ->
					NextCcmOptions = NewCcmOptions,
					Reply = ok;
				EmbeddedReply ->
					NextCcmOptions = CcmOptions,
					Reply = EmbeddedReply
			end,
			ipa_proto:reply(From, Reply),
			ipa_proto:loop(S, StreamMap, NextCcmOptions);
		{ipa_send, S, StreamId, Data} ->
			send(S, StreamId, Data),
			ipa_proto:loop(S, StreamMap, CcmOptions);
		{tcp, S, Data} ->
			% process incoming IPA message and mark socket active once more
			ipa_proto:process_rx_ipa_msg(S, StreamMap, CcmOptions, Data),
			inet:setopts(S, [{active, once}]),
			ipa_proto:loop(S, StreamMap, CcmOptions);
		{tcp_closed, S} ->
			io:format("Socket ~w closed [~w]~n", [S,self()]),
			ipa_proto:process_tcp_closed(S, StreamMap),
			% terminate the process by not looping further
			ok
	end.

% Respond with PONG to PING
process_ccm_msg(Socket, StreamID, _, ping, _) ->
	io:format("Socket ~p Stream ~p: PING -> PONG~n", [Socket, StreamID]),
	send(Socket, StreamID, <<?IPAC_MSGT_PONG>>);
% Respond to ID_ACK with ID_ACK if this instance did not initiate
process_ccm_msg(Socket, StreamID, CcmOptions, id_ack, _) ->
	if
		CcmOptions#ipa_ccm_options.initiate_ack /= true ->
			% Only respond to an ack if this instance did
			% not initiate to prevent an infinite ack loop.
			io:format("Socket ~p Stream ~p: ID_ACK -> ID_ACK~n", [Socket, StreamID]),
			send(Socket, StreamID, <<?IPAC_MSGT_ID_ACK>>);
		true ->
			io:format("Socket ~p Stream ~p: ID_ACK -> None~n", [Socket, StreamID])
	end;
% Simply respond to ID_RESP with ID_ACK
process_ccm_msg(Socket, StreamID, _, id_resp, _) ->
	io:format("Socket ~p Stream ~p: ID_RESP -> ID_ACK~n", [Socket, StreamID]),
	send(Socket, StreamID, <<?IPAC_MSGT_ID_ACK>>);
% Simply respond to ID_GET with ID_RESP
process_ccm_msg(Socket, StreamID, CcmOptions, id_req, _) ->
	io:format("Socket ~p Stream ~p: ID_GET -> ID_RESP~n", [Socket, StreamID]),
	CcmBin = ipa_proto_ccm:encode(
		{id_resp,
		 [{string,serial_nr,CcmOptions#ipa_ccm_options.serial_number},
		  {string,unit_id,CcmOptions#ipa_ccm_options.unit_id},
		  {string,mac_address,CcmOptions#ipa_ccm_options.mac_address},
		  {string,location,CcmOptions#ipa_ccm_options.location},
		  {string,unit_type,CcmOptions#ipa_ccm_options.unit_type},
		  {string,equip_vers,CcmOptions#ipa_ccm_options.equipment_version},
		  {string,sw_version,CcmOptions#ipa_ccm_options.sw_version},
		  {string,unit_name,CcmOptions#ipa_ccm_options.unit_name}
		 ]}),
	send(Socket, StreamID, CcmBin);

% Default message handler for unknown messages
process_ccm_msg(Socket, StreamID, _, MsgType, Opts) ->
	io:format("Socket ~p Stream ~p: Unknown CCM message type ~p Opts ~p~n",
		  [Socket, StreamID, MsgType, Opts]).

% process an incoming CCM message (Stream ID 254)
process_rx_ccm_msg(Socket, StreamID, CcmOptions, PayloadBin) ->
	{MsgType, Opts} = ipa_proto_ccm:decode(PayloadBin),
	process_ccm_msg(Socket, StreamID, CcmOptions, MsgType, Opts).

send_ccm_id_get(Socket) ->
	send(Socket, ?IPAC_PROTO_CCM, <<?IPAC_MSGT_ID_GET>>).

send_ccm_id_ack(Socket) ->
	send(Socket, ?IPAC_PROTO_CCM, <<?IPAC_MSGT_ID_ACK>>).

% convenience wrapper for interactive use / debugging from the shell
listen_accept_handle(LPort, Opts) ->
	case gen_tcp:listen(LPort, ?IPA_SOCKOPTS ++ Opts) of
		{ok, ListenSock} ->
			{ok, Port} = inet:port(ListenSock),
			{ok, Sock} = gen_tcp:accept(ListenSock),
			{ok, IpaPid} = ipa_proto:register_socket(Sock),
			ipa_proto:register_stream(Sock, 0, self()),
			ipa_proto:register_stream(Sock, 255, self()),
			gen_tcp:controlling_process(Sock, IpaPid),
			{ok, Port};
		{error, Reason} ->
			{error, Reason}
	end.

% gen_tcp:connect() convenience wrappers
connect(Address, Port, Options) ->
	connect(Address, Port, Options, infinity).

connect(Address, Port, Options, Timeout) ->
	case gen_tcp:connect(Address, Port, ?IPA_SOCKOPTS ++ Options, Timeout) of
		{ok, Socket} ->
			case ipa_proto:register_socket(Socket) of
				{ok, IpaPid} ->
					{ok, {Socket, IpaPid}};
				{error, Reason} ->
					gen_tcp:close(Socket),
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

% Utility function to continuously server incomming IPA connections on a
% listening TCP socket
start_listen(LPort, NumServers, Opts) ->
	case gen_tcp:listen(LPort, ?IPA_SOCKOPTS ++ Opts) of
		{ok, ListenSock} ->
			start_servers(NumServers, ListenSock, self()),
			{ok, Port} = inet:port(ListenSock),
			Port;
		{error, Reason} ->
			{error, Reason}
	end.

start_servers(0, _, _) ->
	ok;
start_servers(Num, LS, CtrlPid) ->
	spawn(?MODULE, listen_server, [LS, CtrlPid]),
	start_servers(Num-1, LS, CtrlPid).

listen_server(LS, CtrlPid) ->
	case gen_tcp:accept(LS) of
		{ok, S} ->
			io:format("Accepted TCP connection from ~p~n", [inet:peername(S)]),
			% assign the socket to the Controlling process
			gen_tcp:controlling_process(S, CtrlPid),
			CtrlPid ! {ipa_tcp_accept, S},
			listen_server(LS, CtrlPid);
		Other ->
			io:format("accept returned ~w - goodbye!~n", [Other]),
			ok
	end.
