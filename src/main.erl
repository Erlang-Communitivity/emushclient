
-module(main).
-behavior(gen_mush).

-export([start/0]).

-include_lib("wx/include/wx.hrl").

-define(KEEPALIVE_PERIOD,3*60*1000).

-record(state, {top_frame, mush_output_ctrl, cmd_ctrl}).
-record(conn_info, {host,port,char,pass}).
-record(ui, {widget, flags}).

start() ->
       gen_mush:start(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_mush callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
       State = make_window(),
       wxFrame:connect(State#state.top_frame, close_window),
       {ok, State}.
	
groups() -> [mush, mush_gui].

handle_connect_req( _ConnInfo, State) ->
   {ok, State};

handle_send_req(_Char, _Text, State) ->
   {ok, State};

handle_connected( Char, State) ->
   {ok, State}.

handle_connection_lost( _Char, State) ->
   {ok, State}.

handle_received(_Char, Text, State) ->
   Output = State#state.mush_output_ctrl,
   wxTextCtrl:writeText(Output, mush:format(Text)),
   {ok, State}.

handle_shutdown(State) ->   
   {ok, State}.

handle_event(_Event, State) ->
   {ok, State}.

handle_event(wx_event, [#wxClose{}], State) ->
   gen_mush:shutdown(),
   {ok, State};

handle_event(wx_event, [#wxCommand{type=command_text_enter, cmdString=Text}], State) ->
   gen_mush:mush_send(Char, Text),
   {ok, State};
		#wx{event=#wxClose{}} ->
			wxWindow:destroy(State#state.top_frame),
			mush:msg_group(?MODULE, mush, {mush, event, shutdown}),
			loop(State);
		Msg = #wx{event = #wxCommand{type=command_text_enter}} ->
		      	Text = Msg#wx.event#wxCommand.cmdString,
			mush:msg_group(?MODULE, mush, {mush, send, Text}),
			loop(State);

handle_event(_Event, _Args, State) ->
   {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_window() ->
	_Wx = wx:new(),
	Frame = wxFrame:new( wx:null(), -1, "MUSH Client" ),
	TopSplitter   = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),
	wxSplitterWindow:setSashGravity(TopSplitter,   0.75),

	OutputUI = create_output_ui(TopSplitter),
	CommandUI = create_command_ui(TopSplitter),

	wxSplitterWindow:splitHorizontally(TopSplitter, 
						      OutputUI#ui.widget, 
						      CommandUI#ui.widget,
						      [{sashPosition, 600}]),
	wxFrame:setSize(Frame, 800, 800),
	wxFrame:show(Frame),
	#state{top_frame=Frame,
		mush_output_ctrl=OutputUI#ui.widget,
		cmd_ctrl=CommandUI#ui.widget}.

									
create_command_ui(Parent) ->
	SizerFlags = wxSizerFlags:new(),
	CommandTextBox = wxTextCtrl:new(Parent, ?wxID_ANY, [{style, ?wxTE_PROCESS_ENTER}]),
	wxTextCtrl:connect(CommandTextBox, command_text_enter),
	#ui{widget=CommandTextBox, flags=wxSizerFlags:border(SizerFlags,?wxALL, 10)}.


create_output_ui(Parent) ->
	MushOutputText = wxTextCtrl:new(Parent, ?wxID_ANY, [{style, ?wxTE_READONLY bor ?wxTE_MULTILINE}]),
	wxTextCtrl:setSize(MushOutputText,600,750),
	wxTextCtrl:writeText(MushOutputText, "Welcome to EMush Client! Connecting...\n"),
	#ui{widget=MushOutputText, flags= [{flag, ?wxEXPAND bor ?wxALIGN_CENTER}]}.

