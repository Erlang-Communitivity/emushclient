-module(gen_mush).
-behaviour(gen_server).

-export([behavior_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([event/1, event/2, received/2, connected/1, connection_lost/1, mush_connect/1, mush_send/2, shutdown/0, start_link/1, start/1]).

-include_lib("wx/include/wx.hrl").

-record(state, {module, wrapped}).

behavior_info(callbacks) ->
       [
	{init, 0},
	{groups, 0},
        {handle_send_req, 2},
        {handle_connect_req, 2},
        {handle_connected, 2},
	{handle_connection_lost, 2},
	{handle_received,3},
	{handle_event,2 },
	{handle_event, 3},
	{handle_shutdown,1}
     ];
behavior_info(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Module) ->
        gen_server:start_link(?MODULE, [Module], []).

start(Module) ->
        gen_server:start(?MODULE, [Module], []).

connected(Char) ->
       event(connected, [Char]),
       ok.

connection_lost(Char) ->
       event(connection_lost, [Char]),
       ok.

received(Char, Text) ->
	event(received, [Text, Char]),
	ok.
 
mush_connect(ConnInfo) ->
        event(connect_req, [ConnInfo]).

mush_send(Char, Text) ->
        event(send_req, [Text, Char]).

shutdown() ->
	event(shutdown),
	ok.
 
event(Event) when is_atom(Event) ->
	msg_group(mush, {mush, Event, []}),
	ok.
 
event(Event, Args) when is_atom(Event), is_list(Args) ->
	msg_group(mush, {mush, Event, Args}),
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Module]) ->
      {ok, WrappedState} = Module:init(),
      group_setup(Module:groups(), self()),      
      {ok, #state{wrapped=WrappedState, module=Module}}.
 
handle_call(Msg, _From, State) ->
       {noreply, Msg, State}.

handle_cast({mush, event, shutdown, []}, State) -> 
      {stop, shutdown, State};
 
handle_cast({mush, event, received, [Text, Char]}, State=#state{wrapped=Wrapped, module=Module}) -> 
      {ok, WrappedNext} = Module:handle_received(Char, Text, Wrapped),
      {noreply, State#state{wrapped=WrappedNext}};
 
handle_cast({mush, event, send_req, [Text, Char]}, State=#state{wrapped=Wrapped, module=Module}) -> 
      {ok, WrappedNext} = Module:handle_send_req(Char, Text, Wrapped),
      {noreply, State#state{wrapped=WrappedNext}};

handle_cast({mush, event, connect_req, [ConnInfo]}, State=#state{wrapped=Wrapped, module=Module}) -> 
      {ok, WrappedNext} = Module:handle_connect_req(ConnInfo, Wrapped),
      {noreply, State#state{wrapped=WrappedNext}};
 
handle_cast({mush, event, connection_lost, [Char]}, State=#state{wrapped=Wrapped, module=Module}) -> 
      {ok, WrappedNext} = Module:handle_connection_lost(Char, Wrapped),
      {noreply, State#state{wrapped=WrappedNext}};
 
handle_cast({mush, event, connected, [Char]}, State=#state{wrapped=Wrapped, module=Module}) -> 
      {ok, WrappedNext} = Module:handle_connected(Char, Wrapped),
      {noreply, State#state{wrapped=WrappedNext}};

handle_cast({mush, event, Event, []}, State=#state{wrapped=Wrapped, module=Module}) ->
      {ok, WrappedNext} = Module:handle_event(Event, Wrapped),
      {noreply, State#state{wrapped=WrappedNext}};
     
handle_cast({mush, event, Event, Args}, State=#state{wrapped=Wrapped, module=Module}) ->
      {ok, WrappedNext} = Module:handle_event(Event, Args, Wrapped),
      {noreply, State#state{wrapped=WrappedNext}}.

handle_info(#wx{event = Event} , State=#state{wrapped=Wrapped, module=Module}) ->
      {ok, WrappedNext} = Module:handle_event(wx_event, [Event], Wrapped),
      {noreply, State#state{wrapped=WrappedNext}};

handle_info(_Msg, State) -> 		  
      {noreply, State}.

terminate(_Reason, #state{wrapped=Wrapped,module=Module}) -> 
	lists:foreach(fun(G) -> pg2:leave(G, self()) end, Module:groups()),
	Module:handle_shutdown(Wrapped),
	ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
msg_group(Group, Msg) ->
	Pids = pg2:get_members(Group),
	lists:foreach(fun (Pid) ->
			  log_snd(Pid, Msg),
			  gen_server:cast(Pid, Msg) 
			end, Pids).

group_setup(GroupList, Pid) ->
	ProcessGroups = pg2:which_groups(),
	GroupsToCreate = lists:dropwhile(fun(E)-> lists:member(E, ProcessGroups) end, GroupList),
	lists:foreach(fun (E) -> pg2:create(E) end, GroupsToCreate),
	lists:foreach(fun (E) -> pg2:join(E, Pid) end, GroupList).

log_snd( DestPid, {mush, received, _Text, Char}) ->
	 io:format("to ~p: Sending-> ~p~n",[DestPid, {mush, received, "...", Char}]);
log_snd( DestPid, Msg) ->
	 io:format("to ~p: Sending-> ~p~n",[DestPid, Msg]).
