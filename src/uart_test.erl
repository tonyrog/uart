%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     uart test
%%% @end
%%% Created :  2 Feb 2012 by Tony Rogvall <tony@rogvall.se>

-module(uart_test).

-compile(export_all).

tty(a) ->
    "/dev/tty.usbserial-FTF5DP2J";
tty(b) ->
    "/dev/tty.usbserial-FTFBXORB".

options() ->
    {ok,A} = open(a),
    {ok,Opts} = uart:getopts(A, uart:options()),
    uart:close(A),
    Opts.

%%
%% Test "all" combination of:
%%   {active,false|true|once} x {packet,-8...8}     = 3*17 = 51
%%   {active,false|true|once} x {packet,{size,1|5}} = 3*2  = 6 
%%   {active,false|true|once} x {packet,0|line}     = 3*2  = 6 
%%
%% TODO: 
%%   {deliver, port}
%%   {mode,binary}
%%   {header, HSz}
%%
run() ->
    encode_decode(),
    {ok,A} = open(a),
    {ok,B} = open(b),
    transfer_test(A,B,false),
    transfer_test(A,B,true),
    transfer_test(A,B,once),
    uart:close(A),
    uart:close(B),
    ok.

open(X) ->
    uart:open(tty(X), []).

transfer_test(A,B,Active) ->
    Data = "Hello World",
    Len  = length(Data),

    %% Formatting of sender packet + raw reception
    lists:foreach(
      fun(Sz) ->
	      Hdr =
		  if Sz =:= 0 -> [];
		     Sz < 0 -> binary_to_list(<<Len:(-Sz*8)/little>>);
		     true -> binary_to_list(<<Len:(Sz*8)>>)
		  end,
	      Match = Hdr ++ Data,
	      transfer(A, [{packet,Sz}], Data, 
		       B, [{active,Active},{packet,0}], Match)
      end, lists:seq(-8,8)),

    %% Packet reception
    lists:foreach(
      fun(Sz) ->
	      transfer(A, [{packet,Sz}], Data, 
		       B, [{active,Active},{packet,Sz}], Data)
      end, lists:seq(-8,8)),


    %% Line packet
    transfer(A, [{packet,line}], Data++"\n", 
	     B, [{active,Active},{packet,0}], Data++"\n"),
    transfer(A, [{packet,line}], Data++"\n", 
	     B, [{active,Active},{packet,line}], Data++"\n"),

    %% Fixed size packet
    transfer(A, [{packet,0}], "Hello", 
	     B, [{active,Active},{packet,{size,5}}], "Hello"),
    transfer(A, [{packet,0}], "W", 
	     B, [{active,Active},{packet,{size,1}}], "W"),
    ok.


transfer(A, AOpts, Data, B, BOpts, Match) ->
    io:format("Transfer: A=~w, Data=~p, B=~w, Match=~p\n",
	      [AOpts,Data,BOpts,Match]),
    uart:setopts(A, AOpts),
    uart:setopts(B, BOpts),
    uart:send(A, Data),
    Active = proplists:get_value(active,BOpts,false),
    case lists:member({packet,0}, BOpts) of
	true ->
	    recv_loop(B, Active, Match);
	false ->
	    recv(B, Active, Match)
    end.
	
recv_loop(_B, _Active, []) ->
    true;
recv_loop(B, true, Match) ->
    receive
	{uart, B, Data} ->
	    case lists:prefix(Data, Match) of
		true ->
		    recv_loop(B, true, Match--Data);
		false ->
		    exit({bad_match,Data})
	    end
    after 100 ->
	    exit(receive_timeout)
    end;
recv_loop(B, once, Match) ->
    receive
	{uart, B, Data} ->
	    case lists:prefix(Data, Match) of
		true ->
		    {ok,false} = uart:getopt(B, active),
		    uart:setopt(B, active, once),
		    recv_loop(B, once, Match--Data);
		false ->
		    exit({bad_match,Data})
	    end
    after 100 ->
	    exit(receive_timeout)
    end;
recv_loop(B, false, Match) ->
    case uart:recv(B, 0, 10) of
	{ok, Data} ->
	    case lists:prefix(Data, Match) of
		true ->
		    recv_loop(B, false, Match--Data);
		false ->
		    exit({bad_match,Data})
	    end;
	Error ->
	    exit(Error)
    end.

recv(B, Active, Match) when Active =:= true; Active =:= once ->
    receive
	{uart, B, Match} ->
	    true;
	{uart, B, Data} ->
	    exit({bad_match,Data})
    after 100 ->
	    exit(receive_timeout)
    end;
recv(B, false, Match) ->
    case uart:recv(B, 0, 100) of
	{ok, Match} ->
	    true;
	{ok, Data} ->
	    exit({bad_match,Data});
	Error ->
	    exit(Error)
    end.



%%
%% TEST 
%%   options encode/decode
%%

encode_decode() ->
    Success =
	[{device,"COM1:"},
	 {ibaud, 0}, {ibaud, 19200}, {ibaud, 115200},
	 {obaud, 0}, {obaud, 19200}, {obaud, 115200},
	 {csize, 5}, {csize, 6}, {csize, 7}, {csize,8},
	 {bufsz, 1}, {bufsz, 1024},
	 {buftm, 250},
	 {stopb, 1}, {stopb,2}, {stopb,3},
	 {parity,none},{parity,odd},{parity,even},{parity,mark},
	 {hwflow,true},{hwflow,false},
	 {swflow,true},{swflow,false},
	 {xonchar,$\^S},{xonchar,0},
	 {xoffchar,$\^Q},{xoffchar,0},
	 {eolchar,$\n}, {eolchar,$;},
	 {eol2char,$\r}, {eolchar,$:},
	 {active,true},{active,false},{active,once},
	 {delay_send,true},{delay_send,false},
	 {header,0},{header,10},
	 {packet,0},
	 {packet,1},{packet,2},{packet,3},{packet,4},
	 {packet,5},{packet,6},{packet,7},{packet,8},
	 {packet,-1},{packet,-2},{packet,-3},{packet,-4},
	 {packet,-5},{packet,-6},{packet,-7},{packet,-8},
	 {packet,{size,16}},{packet,{size,1}},{packet,{size,64}},
	 {packet,asn1},{packet,sunrm},{packet,cdr},
	 {packet,fcgi},{packet,line},{packet,tpkt},
	 {packet,http},{packet,httph},{packet,ssl_tls},
	 {packet,http_bin},{packet,httph_bin},
	 {packet_size,1024},
	 {deliver,term},{deliver,port},
	 {mode,list},{mode,binary},
	 {high_watermark, 10}, {high_watermark, 255},
	 {low_watermark, 10}, {low_watermark, 255},
	 {send_timeout, -1}, {send_timeout, 10}, 
	 {send_timeout_close, -1}, {send_timeout_close, 0}, 
	 {send_timeout_close, 1000},
	 {buffer,0}, {buffer,1}, {buffer,1024*64},
	 {but8,on}, {bit8,off}, {bit8,set}, {bit8,clear},
	 {exit_on_close,true},	 {exit_on_close,false}
	],
    Fail =
	[{device, 123},
	 {ibaud, -1}, {ibaud,x}, {ibaud,1000000000000},
	 {obaud, -1}, {obaud,x}, {obaud,1000000000000},
	 {csize,x}, {csize,10}, 
	 {bufsz, -1},
	 {buftm, 1000000000000},
	 {buftm, -1},
	 {stopb, 4}, {stopb, x},
	 {parity, 15}, {parity, x},
	 {hwflow, 12}, 
	 {swflow, x},
	 {xonchar, 256},{xonchar,x},
	 {xoffchar, 256},{xoffchar,x},
	 {eolchar, 256},{eolchar,x},
	 {eol2char, 256},{eol2char,x},
	 {active,x},{active,1},
	 {delay_send,5},
	 {packet, 9},
	 {packet, -9},
	 {packet, {size,z}},
	 {packet, {size,16#10000}},
	 {deliver, beer},{deliver,1},
	 {mode,sleep}, {mode,18},
	 {buffer,-1},
	 {but8,x},
	 {exit_on_close,x}
	],

    lists:foreach(
      fun({Opt,Value}) ->
	      Bin = uart:encode_opt(Opt,Value),
	      [{Opt,Value}] = uart:decode_opts(Bin,[])
      end, Success),
    
    lists:foreach(
      fun({Opt,Value}) ->
	      try uart:encode_opt(Opt,Value) of
		  _X -> exit({bad_encoding,{Opt,Value}})
	      catch
		  error:_ ->
		      ok
	      end
      end, Fail).
