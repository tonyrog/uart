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
    "/dev/tty.usbserial-FTFBXORB";
tty(pty) ->
    "pty";
tty(Name) when is_list(Name) ->
    Name.



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
%%   x
%%   {mode,list|binary} = 2
%%   
%%  Sofar = 51*2+6*2+6*2 = 126
%%
%% TODO: 
%%   {deliver, term|port}
%%   {header,  0,1,2,3,4,7,12,13}
%%
run() ->
    run(ab).

run(ab) ->
    encode_test(),
    {ok,A} = open(a),
    {ok,B} = open(b),
    run_ab(A,B),
    uart:close(A),
    uart:close(B),
    ok;
run(pty) ->
    run_pty().

run_pty() ->
    encode_test(),
    {ok,A} = open(pty),
    {ok,TTY} = uart:getopt(A, device),
    {ok,B} = open(TTY),
    run_ab(A,B),
    uart:close(A),
    uart:close(B),
    ok.    

    
run_ab(A,B) ->
    transfer_test(A,B,[{mode,list},{active,false}]),
    transfer_test(A,B,[{mode,list},{active,true}]),
    transfer_test(A,B,[{mode,list},{active,once}]),
    transfer_test(A,B,[{mode,binary},{active,false}]),
    transfer_test(A,B,[{mode,binary},{active,true}]),
    transfer_test(A,B,[{mode,binary},{active,once}]),
    %% modem_test(A, B),
    ok.

open(X) ->
    uart:open(tty(X), []).

transfer_test(A,B,Opts) ->
    Data = "Hello World",
    Len  = length(Data),
    transfer_raw(A,B,Opts,Data,Len,0),
    transfer_raw(A,B,Opts,Data,Len,1),
    transfer_raw(A,B,Opts,Data,Len,2),
    transfer_raw(A,B,Opts,Data,Len,3),
    transfer_raw(A,B,Opts,Data,Len,4),
    transfer_raw(A,B,Opts,Data,Len,5),
    transfer_raw(A,B,Opts,Data,Len,6),
    transfer_raw(A,B,Opts,Data,Len,7),
    transfer_raw(A,B,Opts,Data,Len,8),
    transfer_raw(A,B,Opts,Data,Len,-1),
    transfer_raw(A,B,Opts,Data,Len,-2),
    transfer_raw(A,B,Opts,Data,Len,-3),
    transfer_raw(A,B,Opts,Data,Len,-4),
    transfer_raw(A,B,Opts,Data,Len,-5),
    transfer_raw(A,B,Opts,Data,Len,-6),
    transfer_raw(A,B,Opts,Data,Len,-7),
    transfer_raw(A,B,Opts,Data,Len,-8),

    transfer_packet(A,B,Opts,Data,Len,0),
    transfer_packet(A,B,Opts,Data,Len,1),
    transfer_packet(A,B,Opts,Data,Len,2),
    transfer_packet(A,B,Opts,Data,Len,3),
    transfer_packet(A,B,Opts,Data,Len,4),
    transfer_packet(A,B,Opts,Data,Len,5),
    transfer_packet(A,B,Opts,Data,Len,6),
    transfer_packet(A,B,Opts,Data,Len,7),
    transfer_packet(A,B,Opts,Data,Len,8),
    transfer_packet(A,B,Opts,Data,Len,-1),
    transfer_packet(A,B,Opts,Data,Len,-2),
    transfer_packet(A,B,Opts,Data,Len,-3),
    transfer_packet(A,B,Opts,Data,Len,-4),
    transfer_packet(A,B,Opts,Data,Len,-5),
    transfer_packet(A,B,Opts,Data,Len,-6),
    transfer_packet(A,B,Opts,Data,Len,-7),
    transfer_packet(A,B,Opts,Data,Len,-8),

    transfer_line_packet(A,B,Opts,Data,Len),

    transfer_fixed_packet(A,B,Opts,Data,Len),
    ok.

transfer_raw(A,B,MatchOpts,Data,Len,Sz) ->
    %% Formatting of sender packet + raw reception
    Hdr =
	if Sz =:= 0 -> [];
	   Sz < 0 -> binary_to_list(<<Len:(-Sz*8)/little>>);
	   true -> binary_to_list(<<Len:(Sz*8)>>)
	end,
    Match = Hdr++Data,
    transfer(A, [{packet,Sz}], Data, 
	     B, MatchOpts++[{packet,0}], Match).

transfer_packet(A,B,MatchOpts,Data,_Len,Sz) ->
    %% Packet reception
    transfer(A, [{packet,Sz}], Data, 
	     B, MatchOpts++[{packet,Sz}], Data).


transfer_line_packet(A,B,MatchOpts,Data,_Len) ->
    %% Line packet
    transfer(A, [{packet,line}], Data++"\n", 
	     B, MatchOpts++[{packet,0}], Data++"\n"),
    transfer(A, [{packet,line}], Data++"\n", 
	     B, MatchOpts++[{packet,line}], Data++"\n").

transfer_fixed_packet(A,B,MatchOpts,Data,Len) ->
    %% Fixed size packet
    transfer(A, [{packet,0}], Data, 
	     B, MatchOpts++[{packet,{size,Len}}], Data),
    transfer(A, [{packet,0}], "W", 
	     B, MatchOpts++[{packet,{size,1}}], "W").



transfer(A, AOpts, Data, B, BOpts, Match0) ->
    Match = case proplists:get_value(mode, BOpts, list) of
		list -> Match0;
		binary -> list_to_binary(Match0)
	    end,
    io:format("Transfer: A=~w, Data=~p, B=~w, Match=~999p\n",
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
recv_loop(_B, _Active, <<>>) ->
    true;
recv_loop(B, true, Match) ->
    receive
	{uart, B, Data} ->
	    case match(Data, Match) of
		{true,Match1} -> recv_loop(B, true, Match1);
		true -> true;
		false ->
		    exit({bad_match,Data})
	    end
    after 1000 ->
	    exit(receive_timeout)
    end;
recv_loop(B, once, Match) ->
    receive
	{uart, B, Data} ->
	    case match(Data, Match) of
		{true, Match1} ->
		    {ok,false} = uart:getopt(B, active),
		    uart:setopt(B, active, once),
		    recv_loop(B, once, Match1);
		true -> true;
		false ->
		    exit({bad_match,Data})
	    end
    after 1000 ->
	    exit(receive_timeout)
    end;
recv_loop(B, false, Match) ->
    case uart:recv(B, 0, 100) of
	{ok, Data} ->
	    case match(Data, Match) of
		{true,Match1} ->
		    recv_loop(B, false, Match1);
		true -> true;		
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
    after 1000 ->
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

match(Data, Match) when is_list(Data), is_list(Match) ->
    case lists:prefix(Data, Match) of
	true ->
	    case Match--Data of
		[] -> true;
		More -> {true, More}
	    end;
	false ->
	    false
    end;
match(Data, Match) when is_binary(Data), is_binary(Match) ->
    Sz = byte_size(Data),
    case Match of
	<<Data:Sz/binary, Rest/binary>> ->
	    if byte_size(Rest) =:= 0 ->
		    true;
	       true ->
		    {true, Rest}
	    end;
	_ ->
	    false
    end.

%%
%% Test modem bits
%%
%% A         B
%% DTR  =>   DSR,CD
%% RTS  =>   CTS
%%

modem_test() ->
    {ok,A} = open(a),
    {ok,B} = open(b),
    modem_test(A, B),
    uart:close(A),
    uart:close(B),
    ok.
    

modem_test(A, B) ->
    %% depending on modem pins available on A and B
    %% clear pins
    match_pins(A, B, [], [dtr,rts], []),
    match_pins(B, A, [], [dtr,rts], []),
    %% test RTS: A => B
    match_pins(A, B, [rts], [], [cts]),
    %% test RTS: B => A
    match_pins(B, A, [rts], [], [cts]),
    %% test DTR: A => B
    match_pins(A, B, [dtr], [], [cd,dsr]),
    %% test DTR: B => A
    match_pins(B, A, [dtr], [], [cd,dsr]),

    ok.

match_pins(A, B, SetA,ClrA, MatchB) ->
    uart:set_modem(A,SetA),
    uart:clear_modem(A,ClrA),
    {ok,BPins} = uart:get_modem(B),
    uart:clear_modem(A,SetA),
    io:format("match_pins: SetA=~p,ClrA=~p,MatchB=~p,BPins=~p\n", 
	      [SetA,ClrA,MatchB,BPins]),
    true = (lists:sort(MatchB) =:= lists:sort(BPins -- [ri,dtr,rts])).

%%
%% Test encoding of options
%%

encode_test() ->
    Success =
	[{device,"COM1:"},
	 {ibaud, 0}, {ibaud, 19200}, {ibaud, 115200},
	 {obaud, 0}, {obaud, 19200}, {obaud, 115200},
	 {csize, 5}, {csize, 6}, {csize, 7}, {csize,8},
	 {bufsz, 1}, {bufsz, 1024},
	 {buftm, 250},
	 {stopb, 1}, {stopb,2}, {stopb,3},
	 {parity,none},{parity,odd},{parity,even},{parity,mark},
	 {oflow,[dtr]},{oflow,[sw]},
	 {iflow,[sw]}, {iflow,[cts]},
	 {xonchar,$\^S},{xonchar,0},
	 {xoffchar,$\^Q},{xoffchar,0},
	 {eolchar,$\n}, {eolchar,$;}, {eolchar,$:},
	 {active,true},{active,false},{active,once},
	 {delay_send,true},{delay_send,false},
	 {header,0},{header,10},
	 {packet,0},
	 {packet,1},{packet,2},{packet,3},{packet,4},
	 {packet,5},{packet,6},{packet,7},{packet,8},
	 {packet,-1},{packet,-2},{packet,-3},{packet,-4},
	 {packet,-5},{packet,-6},{packet,-7},{packet,-8},
	 {packet,{size,16}},{packet,{size,1}},{packet,{size,64}},
	 {packet,line},
	 {packet_size,1024},
	 {deliver,term},{deliver,port},
	 {mode,list},{mode,binary},
	 {high_watermark, 10}, {high_watermark, 255},
	 {low_watermark, 10}, {low_watermark, 255},
	 {send_timeout, -1}, {send_timeout, 10}, 
	 {send_timeout_close, -1}, {send_timeout_close, 0}, 
	 {send_timeout_close, 1000},
	 {buffer,0}, {buffer,1}, {buffer,1024*64},
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
	 {oflow, 12}, 
	 {iflow, [cls]},
	 {xonchar, 256},{xonchar,x},
	 {xoffchar, 256},{xoffchar,x},
	 {eolchar, 256},{eolchar,x},
	 {active,x},{active,1},
	 {delay_send,5},
	 {packet, 9},
	 {packet, -9},
	 {packet, {size,z}},
	 {packet, {size,16#10000}},
	 {deliver, beer},{deliver,1},
	 {mode,sleep}, {mode,18},
	 {buffer,-1},
	 {exit_on_close,x}
	],

    lists:foreach(
      fun({Opt,Value}) ->
	      uart:encode_opt(Opt,Value)
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
