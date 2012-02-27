%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    interface to the uart devices
%%% @end
%%% Created : 29 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(uart).

-export([open/2, close/1]).
-export([send/2]).
-export([recv/2, recv/3, unrecv/2]).
-export([break/2, xon/1, xoff/1]).
-export([get_modem/1, set_modem/2, clear_modem/2]).
-export([options/0]).
-export([setopt/3, setopts/2]).
-export([getopt/2, getopts/2]).

-compile(export_all).  %% testing

-define(UART_CMD_OPEN,      1).
-define(UART_CMD_CONNECT,   2).
-define(UART_CMD_DISCONNECT,3).
-define(UART_CMD_CLOSE,     4).
-define(UART_CMD_XON,       5).
-define(UART_CMD_XOFF,      6).
-define(UART_CMD_BREAK,     7).
-define(UART_CMD_SETOPTS,   8).
-define(UART_CMD_GETOPTS,   9).
-define(UART_CMD_SENDCHAR,  10).
-define(UART_CMD_SEND,      11).
-define(UART_CMD_GET_MODEM, 12).
-define(UART_CMD_SET_MODEM, 13).
-define(UART_CMD_CLR_MODEM, 14).
-define(UART_CMD_UNRECV,    15).
-define(UART_CMD_RECV,      16).

%% Option bits are also used as bit numbers, so do not exceed 32.
-define(UART_OPT_DEVICE, 1).
-define(UART_OPT_IBAUD,  2).
-define(UART_OPT_OBAUD,  3).
-define(UART_OPT_CSIZE,  4).
-define(UART_OPT_BUFSZ, 5).
-define(UART_OPT_BUFTM, 6).
-define(UART_OPT_STOPB,  7).
-define(UART_OPT_PARITY, 8).
-define(UART_OPT_HWFLOW, 9).
-define(UART_OPT_SWFLOW, 10).
-define(UART_OPT_XOFFCHAR, 11).
-define(UART_OPT_XONCHAR,  12).
-define(UART_OPT_EOLCHAR,  13).
-define(UART_OPT_EOL2CHAR, 14).
-define(UART_OPT_ACTIVE,   15).
-define(UART_OPT_DELAY_SEND, 16).
-define(UART_OPT_DELIVER, 17).
-define(UART_OPT_MODE, 18).
-define(UART_OPT_HEADER, 20).
-define(UART_OPT_PACKET, 21).
-define(UART_OPT_PSIZE, 22).
-define(UART_OPT_HIGH,  23).
-define(UART_OPT_LOW, 24).
-define(UART_OPT_SENDTMO, 25).  %% send timeout
-define(UART_OPT_CLOSETMO, 26).  %% send close timeout
-define(UART_OPT_BUFFER,   27).
-define(UART_OPT_BIT8,      28).
-define(UART_OPT_EXITF,     29).

-define(UART_PB_LITTLE_ENDIAN, 16#00008000). %% UART_PB_<n> 
-define(UART_PB_BYTES_MASK,    16#00000F00). %% UART_PB_<n> 0..8 allowed
-define(UART_PB_FIXED_MASK,    16#FFFF0000). %% UART_PB_RAW
-define(UART_PB_TYPE_MASK,     16#000000FF). %% UART_PB_x

-define(UART_PB_RAW,       0).
-define(UART_PB_N,         1).
-define(UART_PB_ASN1,      2).
-define(UART_PB_RM,        3).
-define(UART_PB_CDR,       4).
-define(UART_PB_FCGI,      5).
-define(UART_PB_LINE_LF,   6).
-define(UART_PB_TPKT,      7).
-define(UART_PB_HTTP,      8).
-define(UART_PB_HTTPH,     9).
-define(UART_PB_SSL_TLS,   10).
-define(UART_PB_HTTP_BIN,  11).
-define(UART_PB_HTTPH_BIN, 12).

-define(UART_PASSIVE, 0).
-define(UART_ACTIVE,  1).
-define(UART_ONCE,    2).

-define(UART_PARITY_NONE, 0).
-define(UART_PARITY_ODD,  1).
-define(UART_PARITY_EVEN, 2).
-define(UART_PARITY_MARK, 3).

-define(UART_DELIVER_PORT, 0).
-define(UART_DELIVER_TERM, 1).

-define(UART_MODE_LIST,   0).
-define(UART_MODE_BINARY, 1).

-define(UART_OK,      0).
-define(UART_ERROR,   1).
-define(UART_OPTIONS, 2).

-define(UART_MODEM_DTR,  16#0002).
-define(UART_MODEM_RTS,  16#0004).
-define(UART_MODEM_CTS,  16#0008).
-define(UART_MODEM_DCD,  16#0010).
-define(UART_MODEM_RI,   16#0020).
-define(UART_MODEM_DSR,  16#0040).

-define(UART_BIT8_CLEAR, 0).
-define(UART_BIT8_SET,   1).
-define(UART_BIT8_ON,    2).
-define(UART_BIT8_OFF,   3).


-define(bool(X), if (X) -> 1; true -> 0 end).

options() ->
    [
     device,          %% string()
     baud,            %% = ibaud+obaud
     ibaud,           %% unsigned()
     obaud,           %% unsigned()
     csize,           %% 5,6,7,8
     bufsz,           %% unsigned()
     buftm,           %% timer()  [{packet,0}]
     stopb,           %% 1,2,3
     parity,          %% none,odd,even,mark
     hwflow,          %% boolean()
     swflow,          %% boolean()
     xonchar,         %% byte()
     xoffchar,        %% byte()
     eolchar,         %% byter()
     eol2char,        %% byte()
     active,          %% true,false,once
     delay_send,      %% boolean()
     header,          %% unsigned()
     packet,          %% modes()
     packet_size,     %% unsigned()
     deliver,         %% port | term
     mode,            %% list | binary
     high_watermark,
     low_watermark,
     send_timeout,
     send_timeout_close,
     buffer,
     bit8,
     exit_on_close
    ].


getopt(P, baud) ->
    getopt(P, ibaud);
getopt(P, Opt) ->
    Arg = <<(encode_opt(Opt))>>,
    Reply = erlang:port_control(P, ?UART_CMD_GETOPTS, Arg),
    case decode(Reply) of
	{ok,[{_,Value}]} -> {ok,Value};
	Error -> Error
    end.

getopts(P, Opts) when is_list(Opts) ->
    Opts1 = translate_getopts(Opts),
    Data = << <<(encode_opt(Opt))>> || Opt <- Opts1 >>,
    Reply = erlang:port_control(P, ?UART_CMD_GETOPTS, Data),
    case decode(Reply) of
	{ok, Values} ->
	    {ok, translate_getopts_reply(Opts,Values)};
	Error ->
	    Error
    end.

setopt(P, Opt, Value) ->
    setopts(P, [{Opt,Value}]).

setopts(P, Opts) ->
    Opts1 = translate_set_opts(Opts),
    Data = << <<(encode_opt(Opt,Value))/binary>> || {Opt,Value} <- Opts1 >>,
    Reply = erlang:port_control(P, ?UART_CMD_SETOPTS, Data),
    decode(Reply).

open(DeviceName, Opts) ->
    Path = code:priv_dir(uart),
    Driver = "uart_drv",
    case erl_ddll:load(Path, Driver) of
	ok ->
	    Port = erlang:open_port({spawn_driver, Driver}, [binary]),
	    Opts1 = [{ibaud,9600},{device,DeviceName} | Opts],
	    case setopts(Port, Opts1) of
		ok ->
		    {ok,Port};
		Error ->
		    erlang:port_close(Port),
		    Error
	    end;

	Err={error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    Err
    end.

close(Port) ->
    erlang:port_close(Port).

break(Port,Duration) when is_port(Port),
			  is_integer(Duration), Duration > 0 ->
    Reply = erlang:port_control(Port, ?UART_CMD_BREAK, <<Duration:32>>),
    decode(Reply).

xon(Port) when is_port(Port) ->
    Reply = erlang:port_control(Port, ?UART_CMD_XON, []),
    decode(Reply).

xoff(Port) when is_port(Port) ->
    Reply = erlang:port_control(Port, ?UART_CMD_XOFF, []),
    decode(Reply).

get_modem(Port) ->
    Reply = erlang:port_control(Port, ?UART_CMD_GET_MODEM, []),
    case decode(Reply) of
	{ok,Flags} -> {ok,decode_flags(Flags)};
	Error -> Error
    end.

set_modem(Port, Fs) when is_list(Fs) ->
    Flags = encode_flags(Fs),
    Reply = erlang:port_control(Port, ?UART_CMD_SET_MODEM, <<Flags:32>>),
    decode(Reply).

clear_modem(Port, Fs) when is_list(Fs) ->
    Flags = encode_flags(Fs),
    Reply = erlang:port_control(Port, ?UART_CMD_CLR_MODEM, <<Flags:32>>),
    decode(Reply).

send(Port, [C]) when is_port(Port), is_integer(C), C >= 0, C =< 255 ->
    Reply = erlang:port_control(Port, ?UART_CMD_SENDCHAR, [C]),
    decode(Reply);
send(Port, <<C>>) when is_port(Port) ->
    Reply = erlang:port_control(Port, ?UART_CMD_SENDCHAR, [C]),
    decode(Reply);
send(Port, Data) when is_port(Port),is_list(Data) ->
    true = erlang:port_command(Port, Data),
    ok;
send(Port, Data) when is_port(Port), is_binary(Data) ->
    true = erlang:port_command(Port, Data),
    ok.

unrecv(Port, Data) when is_list(Data); is_binary(Data)  ->
    Res = erlang:port_control(Port, ?UART_CMD_UNRECV, Data),
    decode(Res).
    

recv(Port, Length) ->
    recv0(Port, Length, -1).

recv(Port, Length, infinity) ->
    recv0(Port, Length,-1);

recv(Port, Length, Time) when is_integer(Time) ->
    recv0(Port, Length, Time).

recv0(Port, Length, Time) when is_port(Port), is_integer(Length), Length >= 0 ->
    case async_recv(Port, Length, Time) of
	{ok, Ref} ->
	    receive
		{uart_async, Port, Ref, Status} -> Status;
		{'EXIT', Port, _Reason} ->
		    {error, closed}
	    end;
	Error -> Error
    end.
	     
async_recv(Port, Length, Time) ->
    Res = erlang:port_control(Port, ?UART_CMD_RECV, [<<Time:32,Length:32>>]),
    decode(Res).

translate_set_opts([{baud,B}|Opts]) ->
    [{ibaud,B},{obaud,B}|translate_set_opts(Opts)];
translate_set_opts([Opt|Opts]) ->
    [Opt|translate_set_opts(Opts)];
translate_set_opts([]) ->
    [].

translate_getopts([baud|Opts]) ->
    [ibaud|translate_getopts(Opts)];
translate_getopts([Opt|Opts]) ->
    [Opt|translate_getopts(Opts)];
translate_getopts([]) ->
    [].

translate_getopts_reply([baud|Opts],[{ibaud,B}|Vs]) ->
    [{baud,B}|translate_getopts_reply(Opts,Vs)];
translate_getopts_reply([_Opt|Opts],[V|Vs]) ->
    [V|translate_getopts_reply(Opts,Vs)];
translate_getopts_reply([],[]) ->
    [].


decode(<<?UART_OK>>) ->
    ok;
decode(<<?UART_OK,V:8>>)  -> {ok,V};
decode(<<?UART_OK,V:16>>) -> {ok,V};
decode(<<?UART_OK,V:32>>) -> {ok,V};
decode(<<?UART_ERROR>>) -> {error, unknown};
decode(<<?UART_ERROR,Reason/binary>>) ->
    {error, binary_to_atom(Reason,latin1)};
decode(<<?UART_OPTIONS,Opts/binary>>) ->
    {ok,decode_opts(Opts, [])}.

decode_opts(<<>>, Acc) ->
    lists:reverse(Acc);
decode_opts(Binary, Acc) ->
    {KeyVal,Tail} = decode_opt(Binary),
    io:format("decode_opts: ~w\n", [KeyVal]),
    decode_opts(Tail, [KeyVal|Acc]).


decode_opt(<<?UART_OPT_DEVICE,L,Value:L/binary,Tail/binary>>) ->
    {{device, binary_to_list(Value)}, Tail};
decode_opt(<<?UART_OPT_IBAUD,Value:32, Tail/binary>>) ->
    {{ibaud,Value},Tail};
decode_opt(<<?UART_OPT_OBAUD,Value:32, Tail/binary>>) ->
    {{obaud,Value},Tail};
decode_opt(<<?UART_OPT_CSIZE,Value:32, Tail/binary>>) ->
    {{csize,Value},Tail};
decode_opt(<<?UART_OPT_BUFSZ,Value:32, Tail/binary>>) ->
    {{bufsz,Value},Tail};
decode_opt(<<?UART_OPT_BUFTM,Value:32, Tail/binary>>) ->
    {{buftm,Value},Tail};
decode_opt(<<?UART_OPT_STOPB,Value:32,Tail/binary>>) ->
    {{stopb,Value},Tail};
decode_opt(<<?UART_OPT_PARITY,Value:32,Tail/binary>>) ->
    case Value of
	?UART_PARITY_NONE -> {{parity,none},Tail};
	?UART_PARITY_ODD -> {{parity,odd},Tail};
	?UART_PARITY_EVEN -> {{parity,even},Tail};
	?UART_PARITY_MARK -> {{parity,mark},Tail};
	_ -> {{parity,Value}, Tail}
    end;
decode_opt(<<?UART_OPT_HWFLOW,Value:32,Tail/binary>>) ->
    {{hwflow,(Value =/= 0)},Tail};
decode_opt(<<?UART_OPT_SWFLOW,Value:32,Tail/binary>>) ->
    {{swflow,(Value =/= 0)},Tail};
decode_opt(<<?UART_OPT_XONCHAR,Value:32,Tail/binary>>) ->
    {{xonchar,Value},Tail};
decode_opt(<<?UART_OPT_XOFFCHAR,Value:32,Tail/binary>>) ->
    {{xoffchar,Value},Tail};
decode_opt(<<?UART_OPT_EOLCHAR,Value:32,Tail/binary>>) ->
    {{eolchar,Value},Tail};
decode_opt(<<?UART_OPT_EOL2CHAR,Value:32,Tail/binary>>) ->
    {{eol2char,Value},Tail};
decode_opt(<<?UART_OPT_ACTIVE,Value:32,Tail/binary>>) ->
    case Value of
	?UART_ACTIVE -> {{active,true}, Tail};
	?UART_PASSIVE -> {{active,false}, Tail};
	?UART_ONCE    -> {{active,once}, Tail};
	_ -> {{active,Value},Tail}
    end;
decode_opt(<<?UART_OPT_DELAY_SEND,Value:32,Tail/binary>>) ->
    {{delay_send,(Value =/= 0)},Tail};
decode_opt(<<?UART_OPT_HEADER,Value:32,Tail/binary>>) ->
    {{header,Value},Tail};
decode_opt(<<?UART_OPT_PSIZE,Value:32,Tail/binary>>) ->
    {{packet_size,Value},Tail};
decode_opt(<<?UART_OPT_DELIVER,Value:32,Tail/binary>>) ->
    case Value of
	?UART_DELIVER_PORT -> {{deliver,port},Tail};
	?UART_DELIVER_TERM -> {{deliver,term},Tail};
	_ -> {{deliver,Value},Tail}
    end;
decode_opt(<<?UART_OPT_MODE,Value:32,Tail/binary>>) ->
    case Value of
	?UART_MODE_LIST   -> {{mode,list},Tail};
	?UART_MODE_BINARY -> {{mode,binary},Tail};
	_ -> {{mode,Value}, Tail}
    end;
decode_opt(<<?UART_OPT_PACKET,Value:32,Tail/binary>>) ->
    case (Value band ?UART_PB_TYPE_MASK) of
	?UART_PB_RAW ->
	    case Value bsr 16 of
		0 -> {{packet,0}, Tail};
		N -> {{packet,{size,N}}, Tail}
	    end;
	?UART_PB_N ->
	    case (Value band ?UART_PB_BYTES_MASK) bsr 8 of
		N when N > 0, N =< 8 ->
		    if (Value band ?UART_PB_LITTLE_ENDIAN) =/= 0 ->
			    {{packet,-N}, Tail};
		       true ->
			    {{packet,N}, Tail}
		    end;
		true ->
		    {{packet,{value,Value}},Tail}
	    end;
	?UART_PB_ASN1 -> {{packet,asn1},Tail};
	?UART_PB_RM   -> {{packet,sunrm},Tail};
	?UART_PB_CDR  -> {{packet,cdr}, Tail};
	?UART_PB_FCGI -> {{packet,fcgi},Tail};
	?UART_PB_LINE_LF -> {{packet,line},Tail};
	?UART_PB_TPKT    -> {{packet,tpkt},Tail};
	?UART_PB_HTTP    -> {{packet,http},Tail};
	?UART_PB_HTTPH   -> {{packet,httph},Tail};
	?UART_PB_SSL_TLS -> {{packet,ssl_tls},Tail};
	?UART_PB_HTTP_BIN -> {{packet,http_bin},Tail};
	?UART_PB_HTTPH_BIN -> {{packet,httph_bin},Tail};
	_ ->
	    {{packet,{value,Value}},Tail}
    end;
decode_opt(<<?UART_OPT_HIGH,Value:32,Tail/binary>>) ->
    {{high_watermark,Value},Tail};
decode_opt(<<?UART_OPT_LOW,Value:32,Tail/binary>>) ->
    {{low_watermark,Value},Tail};
decode_opt(<<?UART_OPT_SENDTMO,Value:32,Tail/binary>>) ->
    if Value =:= 16#ffffffff -> 
	    {{send_timeout,-1},Tail};
       true ->
	    {{send_timeout,Value},Tail}
    end;
decode_opt(<<?UART_OPT_CLOSETMO,Value:32,Tail/binary>>) ->
    if Value =:= 16#ffffffff -> 
	    {{send_timeout_close,-1},Tail};
       true ->
	    {{send_timeout_close,Value},Tail}
    end;
decode_opt(<<?UART_OPT_BUFFER,Value:32,Tail/binary>>) ->
    {{buffer, Value}, Tail};
decode_opt(<<?UART_OPT_BIT8,Value:32,Tail/binary>>) ->
    case Value of 
	0 -> {{bit8,clear},Tail};
	1 -> {{bit8,set},Tail};
	2 -> {{bit8,on},Tail};
	3 -> {{bit8,off},Tail}
    end;
decode_opt(<<?UART_OPT_EXITF,Value:32,Tail/binary>>) ->
    {{exit_on_close, Value =/= 0},Tail}.



   
%% @doc
%%    Encode UART option
%% @end
-spec encode_opt(Option::atom(),Value::term()) -> 
			ok | {ok,any()} | {error,any()}.

encode_opt(packet,0) -> 
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,PB) when PB>0, PB=< 8 -> 
    <<?UART_OPT_PACKET, (?UART_PB_N bor (PB bsl 8)):32>>;
encode_opt(packet,PB) when PB<0, PB >= -8 ->
    <<?UART_OPT_PACKET, (?UART_PB_N bor ?UART_PB_LITTLE_ENDIAN bor 
			     ((-PB) bsl 8)):32>>;
encode_opt(packet,{size,N}) when is_integer(N), N > 0, N =< 16#ffff ->
    <<?UART_OPT_PACKET, ((N bsl 16) + ?UART_PB_RAW):32>>;
encode_opt(packet,raw) ->
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,sunrm) ->
    <<?UART_OPT_PACKET, ?UART_PB_RM:32>>;
encode_opt(packet,asn1) ->
    <<?UART_OPT_PACKET, ?UART_PB_ASN1:32>>;
encode_opt(packet,cdr) ->
    <<?UART_OPT_PACKET, ?UART_PB_CDR:32>>;
encode_opt(packet,fcgi) ->
    <<?UART_OPT_PACKET, ?UART_PB_FCGI:32>>;
encode_opt(packet,line) ->
    <<?UART_OPT_PACKET, ?UART_PB_LINE_LF:32>>;
encode_opt(packet,tpkt) ->
    <<?UART_OPT_PACKET, ?UART_PB_TPKT:32>>;
encode_opt(packet,http) ->
    <<?UART_OPT_PACKET, ?UART_PB_HTTP:32>>;
encode_opt(packet,httph) -> 
    <<?UART_OPT_PACKET, ?UART_PB_HTTPH:32>>;
encode_opt(packet,http_bin) -> 
    <<?UART_OPT_PACKET, ?UART_PB_HTTP_BIN:32>>;
encode_opt(packet,httph_bin) ->
    <<?UART_OPT_PACKET, ?UART_PB_HTTPH_BIN:32>>;
encode_opt(packet,ssl) ->
    <<?UART_OPT_PACKET, ?UART_PB_SSL_TLS:32>>;
encode_opt(packet,ssl_tls) ->
    <<?UART_OPT_PACKET, ?UART_PB_SSL_TLS:32>>;

encode_opt(device,Name) when is_list(Name); is_binary(Name) ->
    Bin = iolist_to_binary(Name),
    Len = byte_size(Bin),
    <<?UART_OPT_DEVICE,Len,Bin/binary>>;
encode_opt(ibaud,X) when X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_IBAUD,X:32>>;
encode_opt(obaud,X) when X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_OBAUD,X:32>>;
encode_opt(csize,X) when X >= 5, X =< 8 ->
    <<?UART_OPT_CSIZE,X:32>>;
encode_opt(bufsz,X) when X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_BUFSZ,X:32>>;
encode_opt(buftm,X) when X >= 0,X =< 16#ffffffff ->
    <<?UART_OPT_BUFTM,X:32>>;
encode_opt(stopb,Value) when Value >= 1, Value =< 3 ->
    <<?UART_OPT_STOPB,Value:32>>;
encode_opt(parity,none) ->
    <<?UART_OPT_PARITY,?UART_PARITY_NONE:32>>;
encode_opt(parity,odd) ->
    <<?UART_OPT_PARITY,?UART_PARITY_ODD:32>>;
encode_opt(parity,even) ->
    <<?UART_OPT_PARITY,?UART_PARITY_EVEN:32>>;
encode_opt(parity,mark) ->
    <<?UART_OPT_PARITY,?UART_PARITY_MARK:32>>;
encode_opt(hwflow,Value) when is_boolean(Value) ->
    <<?UART_OPT_HWFLOW,?bool(Value):32>>;
encode_opt(swflow,Value) when is_boolean(Value) ->
    <<?UART_OPT_SWFLOW,?bool(Value):32>>;
encode_opt(xonchar,Value) when Value >= 0, Value =< 255 ->
    <<?UART_OPT_XONCHAR,Value:32>>;
encode_opt(xoffchar,Value) when Value >= 0, Value =< 255 ->
    <<?UART_OPT_XOFFCHAR,Value:32>>;
encode_opt(eolchar,Value) when Value >= 0, Value =< 255 ->
    <<?UART_OPT_EOLCHAR,Value:32>>;
encode_opt(eol2char,Value) when Value >= 0, Value =< 255 ->
    <<?UART_OPT_EOL2CHAR,Value:32>>;
encode_opt(active,true) ->   <<?UART_OPT_ACTIVE,?UART_ACTIVE:32>>;
encode_opt(active,false) ->   <<?UART_OPT_ACTIVE,?UART_PASSIVE:32>>;
encode_opt(active,once) ->   <<?UART_OPT_ACTIVE,?UART_ONCE:32>>;

encode_opt(delay_send,X) when is_boolean(X) ->
    <<?UART_OPT_DELAY_SEND,?bool(X):32>>;
encode_opt(header,X) when is_integer(X), X >= 0 ->
    <<?UART_OPT_HEADER, X:32>>;
encode_opt(packet_size,X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_PSIZE, X:32>>;    
encode_opt(deliver,port) ->
    <<?UART_OPT_DELIVER, ?UART_DELIVER_PORT:32>>;
encode_opt(deliver,term) ->
    <<?UART_OPT_DELIVER, ?UART_DELIVER_TERM:32>>;
encode_opt(mode,list) ->
    <<?UART_OPT_MODE, ?UART_MODE_LIST:32>>;
encode_opt(mode,binary) ->
    <<?UART_OPT_MODE, ?UART_MODE_BINARY:32>>;

encode_opt(packet,0) -> 
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,PB) when PB>0, PB=< 8 -> 
    <<?UART_OPT_PACKET, (?UART_PB_N bor (PB bsl 8)):32>>;
encode_opt(packet,PB) when PB<0, PB >= -8 ->
    <<?UART_OPT_PACKET, (?UART_PB_N bor ?UART_PB_LITTLE_ENDIAN bor 
			     ((-PB) bsl 8)):32>>;
encode_opt(packet,{size,N}) when is_integer(N), N > 0, N =< 16#ffff ->
    <<?UART_OPT_PACKET, ((N bsl 16) + ?UART_PB_RAW):32>>;
encode_opt(packet,raw) ->
    <<?UART_OPT_PACKET, ?UART_PB_RAW:32>>;
encode_opt(packet,sunrm) ->
    <<?UART_OPT_PACKET, ?UART_PB_RM:32>>;
encode_opt(packet,asn1) ->
    <<?UART_OPT_PACKET, ?UART_PB_ASN1:32>>;
encode_opt(packet,cdr) ->
    <<?UART_OPT_PACKET, ?UART_PB_CDR:32>>;
encode_opt(packet,fcgi) ->
    <<?UART_OPT_PACKET, ?UART_PB_FCGI:32>>;
encode_opt(packet,line) ->
    <<?UART_OPT_PACKET, ?UART_PB_LINE_LF:32>>;
encode_opt(packet,tpkt) ->
    <<?UART_OPT_PACKET, ?UART_PB_TPKT:32>>;
encode_opt(packet,http) ->
    <<?UART_OPT_PACKET, ?UART_PB_HTTP:32>>;
encode_opt(packet,httph) -> 
    <<?UART_OPT_PACKET, ?UART_PB_HTTPH:32>>;
encode_opt(packet,http_bin) -> 
    <<?UART_OPT_PACKET, ?UART_PB_HTTP_BIN:32>>;
encode_opt(packet,httph_bin) ->
    <<?UART_OPT_PACKET, ?UART_PB_HTTPH_BIN:32>>;
encode_opt(packet,ssl) ->
    <<?UART_OPT_PACKET, ?UART_PB_SSL_TLS:32>>;
encode_opt(packet,ssl_tls) ->
    <<?UART_OPT_PACKET, ?UART_PB_SSL_TLS:32>>;
encode_opt(high_watermark,X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_HIGH, X:32>>;    
encode_opt(low_watermark,X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_LOW, X:32>>;
encode_opt(send_timeout,X) when is_integer(X),X >= -1, X =< 16#7fffffff ->
    <<?UART_OPT_SENDTMO, X:32>>;
encode_opt(send_timeout_close,X) when is_integer(X),X >= -1, X =< 16#7fffffff ->
    <<?UART_OPT_CLOSETMO, X:32>>;
encode_opt(buffer, X) when is_integer(X), X >= 0, X =< 16#ffffffff ->
    <<?UART_OPT_BUFFER, X:32>>;    
encode_opt(bit8, clear) ->
    <<?UART_OPT_BIT8, ?UART_BIT8_CLEAR:32>>;
encode_opt(bit8, set) ->
    <<?UART_OPT_BIT8, ?UART_BIT8_SET:32>>;
encode_opt(bit8, on) ->
    <<?UART_OPT_BIT8, ?UART_BIT8_ON:32>>;
encode_opt(bit8, off) ->
    <<?UART_OPT_BIT8, ?UART_BIT8_OFF:32>>;
encode_opt(exit_on_close, X) when is_boolean(X) ->
    <<?UART_OPT_EXITF,?bool(X):32>>.


encode_opt(device) -> ?UART_OPT_DEVICE;
encode_opt(ibaud)  -> ?UART_OPT_IBAUD;
encode_opt(obaud)  -> ?UART_OPT_OBAUD;
encode_opt(csize)  -> ?UART_OPT_CSIZE;
encode_opt(bufsz) -> ?UART_OPT_BUFSZ;
encode_opt(buftm) -> ?UART_OPT_BUFTM;
encode_opt(stopb) -> ?UART_OPT_STOPB;
encode_opt(parity) -> ?UART_OPT_PARITY;
encode_opt(hwflow) -> ?UART_OPT_HWFLOW;
encode_opt(swflow) -> ?UART_OPT_SWFLOW;
encode_opt(xonchar) -> ?UART_OPT_XONCHAR;
encode_opt(xoffchar) -> ?UART_OPT_XOFFCHAR;
encode_opt(eolchar) ->  ?UART_OPT_EOLCHAR;
encode_opt(eol2char) -> ?UART_OPT_EOL2CHAR;
encode_opt(active) -> ?UART_OPT_ACTIVE;
encode_opt(delay_send) -> ?UART_OPT_DELAY_SEND;
encode_opt(header)     -> ?UART_OPT_HEADER;
encode_opt(packet) ->  ?UART_OPT_PACKET;
encode_opt(packet_size) ->  ?UART_OPT_PSIZE;
encode_opt(deliver)     ->  ?UART_OPT_DELIVER;
encode_opt(mode)     ->  ?UART_OPT_MODE;
encode_opt(high_watermark) -> ?UART_OPT_HIGH;
encode_opt(low_watermark) -> ?UART_OPT_LOW;
encode_opt(send_timeout) -> ?UART_OPT_SENDTMO;
encode_opt(send_timeout_close) -> ?UART_OPT_CLOSETMO;
encode_opt(buffer) -> ?UART_OPT_BUFFER;
encode_opt(bit8) -> ?UART_OPT_BIT8;
encode_opt(exit_on_close) -> ?UART_OPT_EXITF.
    
     
encode_flags([F|Fs]) ->
    encode_flag(F) + encode_flags(Fs);
encode_flags([]) ->
    0.

encode_flag(dtr) -> ?UART_MODEM_DTR;
encode_flag(rts) -> ?UART_MODEM_RTS;
encode_flag(cts) -> ?UART_MODEM_CTS;
encode_flag(dcd) -> ?UART_MODEM_DCD;
encode_flag(ri)  -> ?UART_MODEM_RI;
encode_flag(dsr) -> ?UART_MODEM_DSR.

decode_flags(Flags) when is_integer(Flags) ->
    if Flags band ?UART_MODEM_DTR =/= 0 -> [dtr]; true -> [] end ++
    if Flags band ?UART_MODEM_RTS =/= 0 -> [rts]; true -> [] end ++
    if Flags band ?UART_MODEM_CTS =/= 0 -> [cts]; true -> [] end ++
    if Flags band ?UART_MODEM_DCD =/= 0 -> [dcd]; true -> [] end ++
    if Flags band ?UART_MODEM_RI  =/= 0 -> [ri]; true -> [] end ++
    if Flags band ?UART_MODEM_DSR =/= 0 -> [dsr]; true -> [] end.

