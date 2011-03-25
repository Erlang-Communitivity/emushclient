-module(mc).
-behavior(gen_mush).

-export([start/0]).

-record(state, {conn, conn_info}).
-record(conn_info, {host,port,char,pass}).
-define(GROUPS, [mush, mush_client]).

start() ->
       gen_mush:start(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_mush callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
       {ok, #state{}}.
	
groups() -> [mush, mush_client].

handle_connect_req( ConnInfo, State) ->
   #conn_info{host=Host,port=Port, char=Char, pass=Pass} = ConnInfo,   
   {ok, Conn} = connect(Host, Port, Char, Pass),
   gen_mush:connected(Char),
   {ok, State#state{conn=Conn,conn_info=ConnInfo}}.
   
handle_send_req(_Char, Text, State) ->
   say(State#state.conn, Text),
   {ok, State}.

handle_connected( _Char, State) ->
   {ok, State}.

handle_connection_lost( _Char, State) ->
   {ok, State}.

handle_received(Char, Text, State) ->
   {ok, State}.

handle_shutdown(State) ->   
   close(State#state.conn),
   {ok, State}.

handle_event(_Event, State) ->
   {ok, State}.

handle_event(_Event, _Args, State) ->
   {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(Server, Port, Char, Pass) ->
	case gen_tcp:connect(Server, Port, [list, inet, {packet, raw}], 3000) of
		{ok, Socket} ->
			Receiver = connect_receiver(Socket, Char),
			Conn = {conn, Socket, Receiver},
			say(Conn, "connect " ++ Char ++ " " ++ Pass),			
			{ok, Conn };
		{error, Reason} -> {error, Reason}
	end.
	
say(ConnectRecord, Data) ->
	{conn, Socket, _} = ConnectRecord,
	gen_tcp:send(Socket, Data ++ "\r\n").

close(ConnectRecord) ->
	{conn, Socket, ReceiverPID} = ConnectRecord,
	ReceiverPID ! die,
	gen_tcp:close(Socket).


receiver(Socket, Char) ->
	receive
		{tcp, Socket, Data} ->
		        gen_mush:received(Char, Data),
			receiver(Socket, Char);
		{tcp_closed, Socket} ->
			gen_mush:connection_lost(Char),
			receiver(Socket, Char);
		die ->
			ok
	end.

connect_receiver(Socket, Char) ->
	Receiver = spawn(fun() -> receiver(Socket, Char) end),
	gen_tcp:controlling_process(Socket, Receiver),
	Receiver.

	
