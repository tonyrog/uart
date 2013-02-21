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

-import(lists, [map/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(uart_device,
	{
	  name :: string(),    %% device name
	  path :: string(),    %% device dir path
	  id   :: string(),    %% internal node name (info)
	  owner :: pid(),      %% owner pid
	  mon  :: reference(), %% process monitor
	  avail                %% true it device is (known to be) available
	}).

-record(state, {
	  dref         :: undefined | reference(),
	  iref         :: undefined | reference(),
	  devices = [] :: [#uart_device{}],
	  sub = []     :: [{pid(),reference()}]
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

subscribe() ->
    gen_server:call(?SERVER, {subscribe,self()}).

unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe,self(),Ref}).
    
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
    {DRef,IRef} = watch_uart_devices(),
    Ds  = list_uart_devices(),
    State0 = #state{ devices =[], dref=DRef, iref=IRef },
    State1 = add_all(Ds, State0),
    {ok, State1}.

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
handle_call({subscribe,Pid}, _From, State) ->
    Ref = erlang:monitor(process,Pid),
    Sub = [{Pid,Ref} | State#state.sub],
    {reply,{ok,Ref},State#state { sub = Sub }};
handle_call({unsubscribe,Pid,Ref}, _From, State) ->
    case lists:keytake(Ref,2,State#state.sub) of
	{value,{Pid,Ref},Sub} ->
	    erlang:demonitor(Ref, [flush]),
	    {reply,ok,State#state { sub = Sub }};
	_ ->
	    {reply,ok,State}
    end;
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
  when Ref =:= State#state.dref ->
    case os:type() of
	{unix,darwin} ->
	    case is_uart_device(Path,Name) of
		true ->
		    Ds0 = State#state.devices,
		    case lists:keyfind(Name,#uart_device.name, Ds0) of
			false ->
			    State1 = add(#uart_device { 
					    name = Name, 
					    path = Path, 
					    id   = Name,
					    avail=true }, State),
			    {noreply, State1};
			_D ->
			    {noreply, State}
		    end;
		false ->
		    {noreply, State}
	    end;
	{unix,linux} ->
	    if Name =:= "serial",
	       State#state.devices =:= [],
	       State#state.iref =:= undefined ->
		    IRef = case fnotify:watch("/dev/serial/by-id") of
			       {ok,Ref} -> Ref;
			       {error,enoent} -> undefined
			   end,
		    Ds = list_uart_devices(),
		    State1 = State#state { iref=IRef },
		    State2 = add_all(Ds, State1),
		    {noreply, State2};
	       true ->
		    {noreply, State}
	    end
    end;
handle_info({fevent,Ref,[create],IPath,ID}, State) 
  when Ref =:= State#state.iref ->
    case os:type() of
	{unix,linux} ->
	    case is_uart_device(IPath,ID) of
		true ->
		    Link = filename:join(IPath,ID),
		    case file:read_link(Link) of
			{ok,Name} ->
			    Name1=filename:basename(Name),
			    D = #uart_device { name=Name1, 
					       path="/dev",
					       id=ID,
					       avail=true },
			    State1 = add(D, State),			 
			    {noreply, State1};
			Error ->
			    io:format("read_link: error ~p\n", [Error]),
			    {noreply, State}
		    end;
		false ->
		    {noreply, State}
	    end;
	_ ->
	    {noreply, State}
    end;
handle_info({fevent,Ref,[delete],_Path,Name}, State) 
  when Ref =:= State#state.dref ->
    case os:type() of
	{unix,darwin} ->
	    State1 = removed_by_name(Name, State),
	    {noreply, State1};
	{unix,linux} ->
	    if Name =:= "serial" ->
		    %% all devices still present are gone
		    unwatch(State#state.iref),
		    State1 = remove_all(State#state.devices,State),
		    {noreply, State1#state { iref=undefined}};
	       true ->
		    {noreply, State}
	    end;
	_ ->
	    {noreply, State}
    end;
handle_info({fevent,Ref,[delete],_IPath,ID}, State) 
  when Ref =:= State#state.iref ->
    case os:type() of
	{unix,darwin} ->
	    State1 = removed_by_id(ID, State),
	    {noreply, State1};
	_ ->
	    {noreply, State}
    end;

handle_info({'DOWN',Ref,process,Pid,_Reason}, State) ->
    case lists:keytake(Ref, #uart_device.mon, State#state.devices) of
	false ->
	    case lists:keytake(Ref,2,State#state.sub) of
		{value,{Pid,Ref},Sub} ->
		    {noreply,State#state { sub = Sub }};
		_ ->
		    {noreply,State}
	    end;
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
	    map(fun(Name) ->
			D = #uart_device { name=Name, 
					   path=Path, 
					   id=Name, avail=true},
			io:format("ADD: ~p\n", [D]),
			D
		end, Ds1);
	{unix,linux} ->
	    case filelib:is_dir("/dev/serial") of
		false ->
		    [];
		true ->
		    Path = "/dev",
		    IPath = "/dev/serial/by-id",
		    case file:list_dir(IPath) of
			{ok,Fs} ->
			    Ds1=
				lists:foldl(
				  fun(ID,Acc) ->
					  Link = filename:join(IPath,ID),
					  case file:read_link(Link) of
					      {ok,Name} ->
						  Name1=filename:basename(Name),
						  [{Name1,ID}|Acc];
					      _ ->
						  Acc
					  end
				  end, [], Fs),
			    map(
			      fun({Name,ID}) ->
				      D=#uart_device { name=Name, 
						       path=Path,
						       id = ID,
						       avail=true},
				      io:format("ADD: ~p\n", [D]),
				      D
			      end, Ds1);
			_Error -> %% /dev/serial/... may vanish!
			    []
		    end
	    end;
	_ ->
	    []
    end.


is_uart_device(_Path,Name) ->  %% fixme check fileinfo
    lists:prefix("tty.", Name).

watch_uart_devices() ->
    case os:type() of
	{unix,darwin} ->
	    {ok,DRef} = fnotify:watch("/dev"),
	    {DRef,undefined};
	{unix,linux} ->
	    {ok,DRef} = fnotify:watch("/dev"),
	    case fnotify:watch("/dev/serial/by-id") of
		{ok,Ref} -> 
		    {DRef,Ref};
		{error,enoent} ->
		    {DRef,undefined}
	    end;
	_ ->
	    {undefined,undefined}
    end.

unwatch(undefined) ->
    ok;
unwatch(Ref) ->
    fnotify:unwatch(Ref).

%% device was added (USB pugged in)
add(D,State) ->
    io:format("ADD: ~p\n", [D]),
    lists:foreach(fun({Pid,Ref}) -> 
			  Pid ! {uart_device,Ref,[added],D#uart_device.name}
		  end, State#state.sub),
    Ds = State#state.devices,
    State#state { devices = [D|Ds]}.

add_all([D|Ds],State) ->
    State1 = add(D, State),
    add_all(Ds, State1);
add_all([], State) ->
    State.
    

%% device is removed (USB device unplugged)
removed_by_name(Name, State) ->
    case lists:keytake(Name, #uart_device.name, State#state.devices) of
	false ->
	    State;
	{value,D,Ds} ->
	    remove(D,Ds,State)
    end.

removed_by_id(ID, State) ->
    case lists:keytake(ID, #uart_device.id, State#state.devices) of
	false ->
	    State;
	{value,D,Ds} ->
	    remove(D,Ds,State)
    end.

remove_all([D|Ds],State) ->
    State1 = remove(D,Ds,State),
    remove_all(Ds, State1);
remove_all([],State) ->
    State.

remove(D,Ds,State) ->
    io:format("REMOVED: ~p\n", [D]),
    if is_pid(D#uart_device.owner) ->
	    %% maybe flag this ?
	    D#uart_device.owner ! {uart_device,self(),[removed],
				   D#uart_device.name};
       true ->
	    ok
    end,
    lists:foreach(fun({Pid,Ref}) -> 
			  Pid ! {uart_device,Ref,[removed],D#uart_device.name}
		  end, State#state.sub),
    if is_reference(D#uart_device.mon) ->
	    erlang:demonitor(D#uart_device.mon,[flush]),
	    ok;
       true ->
	    ok
    end,
    State#state { devices = Ds }.

