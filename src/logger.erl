-module(logger).
-behavior(gen_mush).

-export([start/0, make_dummy/1, dump/1, dump/3, dump_to_html/1]).

-record(state, {char, connected, table, buffer}).

%%%%%
%% This module handles persistant MUSH logging
%% Rather than as a simple text file I want to be able to work with it as a
%% database. For now this is very simple, using a DETS table that gets
%% reversed in memory when we dump it to dump in insertion order. This has
%% many pitfalls and the goal is to transition to something better but 
%% still lightweight, perhaps Bitcask. -wab@2010.dec.19
%%


start() ->
       gen_mush:start(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_mush callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
       {ok, #state{char=none, table=none, connected=false, buffer=[] }}.
	
groups() -> [mush, mush_plugin].

handle_connect_req( _ConnInfo, State) ->
   {ok, State};

handle_send_req(_Char, _Text, State) ->
   {ok, State};

handle_connected( Char, State) ->
   {ok, open_data(State, Char)}.

handle_connection_lost( _Char, State) ->
   {ok, close_data(State)}.

handle_received(Char, Text, State) ->
   {ok, add_data(State, Char, Text)}.

handle_shutdown(State) ->   
   {ok, close_data(State)}.

handle_event(_Event, State) ->
   {ok, State}.

handle_event(_Event, _Args, State) ->
   {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open_data(State, Char) ->
    {_, D} = dets:open_file(Char ++ ".dets", []),
    State#state{char=Char, table=D, connected=true}.

close_data(State) ->
    case State#state.table of
       none ->  State;
       Table -> dets:close(Table),
       	     	#state{char=none, table=none, connected=false}
    end.

add_data(State, Char, Text) ->
    {NextBuffer, Lines} = buffer(State#state.buffer, Text),
    AddFn = fun (Line) ->
            	dets:insert(State#state.table, {now(),  Char, Line})
	    end,    	    	
    lists:foreach(AddFn, Lines),
    State#state{buffer = NextBuffer}.


%%
% This buffers until a \n is found,
% skipping a \r if found
% It takes the current buffer and incoming text.
% It returns a tuple of {NextBuffer, []) if no line is found yet
% or {NextBuffer, [Lines]} if a line is found. Note that Line does not contain \n
% character
%
buffer(Buffer, Text)  ->
     buffer(Buffer, Text, []).
     
buffer(Buffer, [$\n], Lines)   ->
      buffer([], [], [lists:reverse(Buffer) | Lines]);

buffer(Buffer, [$\n | Text], Lines)   ->
      buffer([], Text, [lists:reverse(Buffer) | Lines]);

buffer(Buffer, [Ch | Text], Lines)  -> 
      buffer([Ch | Buffer], Text, Lines);

buffer(Buffer, [], Lines)  ->
      {Buffer, lists:reverse(Lines)}.

make_dummy(Char) ->
      {_, D} = dets:open_file(Char ++ ".dets", []),
      dets:insert(D, {now(),  Char, "First msg\n\nBlah\n"}),
      dets:insert(D, {now(),  Char, "Second msg\n\nBlah\n"}),
      dets:insert(D, {now(),  Char, "Third msg\n\nBlah\n"}),
      dets:close(D).

dump_to_html(Char) ->
      {ok, FileDesc} = file:open(Char ++ ".html", [write]),
      io:format(FileDesc,"<html>~n<head><title>MUSH Log for ~s</title></head>~n<body>~n<table>~n", [Char]),
      ValFun = fun (Row, Acc) -> [Row | Acc] end,
      Rows = dump(Char, ValFun, []),
      RowsInOrder = lists:sort(fun ({TS1, _, _},{TS2, _, _})  -> compare_ts(TS1, TS2) end, Rows),
      lists:foreach(fun ({TS, _Char, Msg}) -> 
      	     	 Row = io_lib:format("<tr><td>~s</td><td><pre>~s</pre><td></tr>",[format_timestamp(TS),mush:format(Msg)]),
	     	 io:format(FileDesc, "~s~n", [Row]) 
	     end, RowsInOrder),
      io:format(FileDesc,"~n</table>~n</body>~n</html>",[]),
      file:close(FileDesc).



compare_ts({Mega1,_,_},{Mega2,_,_}) when Mega1 < Mega2 ->
    true;
compare_ts({Mega1,_,_},{Mega2,_,_}) when Mega1 > Mega2 ->
    false;
compare_ts({_,Secs1,_},{_,Secs2,_}) when Secs1 < Secs2 ->
    true;
compare_ts({_,Secs1,_},{_,Secs2,_}) when Secs1 > Secs2 ->
    false;
compare_ts({_,_,Micro1},{_,_,Micro2}) when Micro1 =< Micro2 ->
    true;
compare_ts({_,_,Micro1},{_,_,Micro2}) when Micro1 > Micro2 ->
    false.



dump(Char) ->
      dump(Char, fun dump_value/2, none).

dump(Char, ValFun, Acc) ->
      {_, D} = dets:open_file(Char ++ ".dets", []),
      AccFinal = dump_key(dets:first(D), D, ValFun, Acc),
      dets:close(D),
      AccFinal.

dump_key('$end_of_table', _Table, _ValFun, Acc) ->
   Acc;
dump_key(Key, Table, ValFun, Acc) ->
   Vals = dets:lookup(Table, Key),
   AccNext = lists:foldl(ValFun, Acc, Vals),
   dump_key(dets:next(Table,Key), Table, ValFun, AccNext).



dump_value({TS, _Char, Msg}, _Acc) ->
   io:format("~s: ~s",[format_timestamp(TS),Msg]).

format_timestamp(TS) ->
    {_,_,_Micro} = TS,
    {{Year,Month,Day},{Hour,Minute,_Second}} = 
	calendar:now_to_universal_time(TS),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
			  "Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w",
		  [Day,Mstr,Year,Hour,Minute]).
