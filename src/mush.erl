
-module(mush).

-export([format/1]).

%%test_conn() ->
%%	    pg2:create(mush),
%%	    spawn(fun() -> mc:start() end),
%%	    ConnInfo = {conn_info,"roadtoamber.com",6250,"Alonzo","xcoLh12X"},
%%	    mush:msg_group(?MODULE, mush, {mush, connect, ConnInfo}).

format(Text) ->
      format(Text, ok, []).
format([], _State, Text) ->
       Text;
format([$\r | Tail], ok, Text) ->
      format(Tail, ok, Text);
format([27 | Tail], ok, Text) ->
      format(Tail, midst, Text);
format([Head | Tail], ok, Text) ->
       format(Tail, ok, Text ++ [Head]);
format([109 | Tail], midst, Text) ->
      format(Tail, ok, Text);
format([_ | Tail], midst, Text) ->
       format(Tail, midst, Text).