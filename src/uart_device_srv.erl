%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Server that keep track on uart/tty devices in the system
%%% @end
%%% Created : 20 Feb 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(uart_device_srv).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/0]).
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(uart_device,
	{
	  name :: string(),    %% device name
	  path :: string(),    %% device dir path
	  owner :: pid(),      %% owner pid
	  mon  :: reference(), %% process monitor
	  avail                %% true it device is (known to be) available
	}).

-record(state, {
	  watch        :: undefined | reference(),
	  devices = [] :: [#uart_device{}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================

alloc(Name) ->
    gen_server:call(?SERVER, {alloc,self(),Name}).

release(Name) ->
    gen_server:call(?SERVER, {release,self(),Name}).

get_list() ->
    gen_server:call(?SERVER, get_list).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Ref = watch_uart_devices(),
    Ds  = list_uart_devices(),
    {ok, #state{ devices = Ds, watch=Ref }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_list, _From, State) ->
    {reply, {ok, [D#uart_device.name || D <- State#state.devices]}, State};
handle_call({alloc,Pid,Name}, _From, State) ->
    case lists:keytake(Name,#uart_device.name,State#state.devices) of
	false ->
	    {reply, {error,enoent}, State};
	{value,D,Ds} ->
	    if not D#uart_device.avail ->
		    {reply, {error,ebusy}, State};
	       true ->
		    Mon = erlang:monitor(process, Pid),
		    D1 = D#uart_device { avail = false,
					 owner = Pid,
					 mon   = Mon },
		    io:format("ALLOC: ~p\n", [D1]),
		    Ds1 = [D1 | Ds],
		    Path = filename:join(D#uart_device.path,D#uart_device.name),
		    {reply, {ok,Path}, State#state { devices = Ds1 }}
	    end
    end;
handle_call({release,Pid,Name}, _From, State) ->
    case lists:keytake(Name,#uart_device.name,State#state.devices) of
	false ->
	    {reply, {error,enoent}, State};
	{value,D,Ds} ->
	    if D#uart_device.avail =:= true,
	       D#uart_device.owner =:= Pid ->
		    io:format("RELEASE: ~p\n", [D]),
		    erlang:demonitor(D#uart_device.mon, [flush]),
		    D1 = D#uart_device { avail = true,
					 owner = undefined,
					 mon   = undefined },
		    Ds1 = [D1 | Ds],
		    {reply, ok, State#state { devices = Ds1 }};
	       true ->
		    {reply, {error,eperm}, State}
	    end
    end;
handle_call(_Request, _From, State) ->
    io:format("Unknown call: ~p\n", [_Request]),
    Reply = {error,bad_call},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({fevent,Ref,[create],Path,Name}, State) 
  when Ref =:= State#state.watch ->
    case is_uart_device(Path,Name) of
	true ->
	    D = #uart_device { name=Name, path=Path, avail=true },
	    io:format("ADD: ~p\n", [D]),
	    Ds = [D | State#state.devices],
	    {noreply, State#state { devices = Ds }};
	false ->
	    {noreply, State}
    end;
handle_info({fevent,Ref,[delete],_Path,Name}, State) 
  when Ref =:= State#state.watch ->
    case lists:keytake(Name, #uart_device.name, State#state.devices) of
	false ->
	    {noreply, State};
	{value,D,Ds} ->
	    io:format("DELETE: ~p\n", [D]),
	    {noreply, State#state { devices = Ds }}
    end;
handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    case lists:keytake(Ref, #uart_device.mon, State#state.devices) of
	false ->
	    {noreply, State};
	{value,D,Ds} ->
	    D1 = D#uart_device { avail = true,
				 owner = undefined,
				 mon   = undefined },
	    io:format("AUTO-RELEASED: ~p\n", [D1]),
	    Ds1 = [D1 | Ds],
	    {noreply,  State#state { devices = Ds1 }}
    end;

handle_info(_Info, State) ->
    io:format("Info: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

list_uart_devices() ->
    case os:type() of
	{unix,darwin} ->
	    Path = "/dev",
	    {ok,Ds} = file:list_dir(Path),
	    Ds1 = lists:filter(fun(D) -> is_uart_device(Path,D) end, Ds),
	    [#uart_device { name=D, path=Path, avail=true} || D <- Ds1];
	_ ->
	    []
    end.

is_uart_device(_Path,Name) ->  %% fixme check fileinfo
    lists:prefix("tty.", Name).

watch_uart_devices() ->
    case os:type() of
	{unix,darwin} ->
	    {ok,Ref} = fnotify:watch("/dev"),
	    Ref;
	_ ->
	    undefined
    end.
