%% Author: Anton Krasovsky 


-module(ssha).
-behaviour(gen_fsm).

-include("ssha.hrl").

-export([start/1, connect/4, send/2, exec/2, close/1]).
-export([init/1, starting/3, reading_banner/2, guess_prompt1/2, guess_prompt2/2, guess_prompt3/2,
         ready/3, waiting/2, waiting_exec/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {connection, prompt, prompt1, prompt2, prompt3, channel, replyto, received, command, id}).

start(Sid) -> gen_fsm:start(?MODULE, [Sid], []).
connect(FsmRef, Host, User, Password) -> gen_fsm:sync_send_event(FsmRef, {connect, Host, User, Password}, ?TIMEOUT). 
send(FsmRef, Command) -> gen_fsm:sync_send_event(FsmRef, {send, Command}, ?TIMEOUT).
exec(FsmRef, Command) -> gen_fsm:sync_send_event(FsmRef, {exec, Command}, ?TIMEOUT).
close(FsmRef) -> gen_fsm:send_all_state_event(FsmRef, close).

%% Server functions
init([Sid]) ->
    gproc:add_local_name(Sid),
    {ok, starting, #state{prompt1 = [], prompt2 = [], prompt3 = [], received = [], command = [], id = Sid}}.

reading_banner(timeout, StateData) ->
    send_cr(StateData),
    {next_state, guess_prompt1, StateData, ?PROMPT_TIMEOUT}.

guess_prompt1(timeout, StateData) ->
    case length(StateData#state.prompt1) of
        0 ->
            gen_fsm:reply(StateData#state.replyto, {error, failed_to_guess_prompt}),
        	{stop, normal, StateData};
		_ ->
			send_cr(StateData),
			{next_state, guess_prompt2, StateData, ?PROMPT_TIMEOUT}
    end.

guess_prompt2(timeout, StateData) ->
    case length(StateData#state.prompt2) of
        0 ->
            gen_fsm:reply(StateData#state.replyto, {error, failed_to_guess_prompt}),
        	{stop, normal, StateData};
		_ ->
			send_cr(StateData),
			{next_state, guess_prompt3, StateData, ?PROMPT_TIMEOUT}
    end.

guess_prompt3(timeout, StateData) ->
    case length(StateData#state.prompt3) of
        0 ->
            gen_fsm:reply(StateData#state.replyto, {error, failed_to_guess_prompt}),
        	{stop, normal, StateData};
		_ ->
			case guess_prompt(hd(StateData#state.prompt1), hd(StateData#state.prompt2), hd(StateData#state.prompt3)) of
                {ok, Prompt} ->
                    ?LOG("prompt ~p~n", [Prompt]),
                    gen_fsm:reply(StateData#state.replyto, ok),
					{next_state, ready, StateData#state{prompt1=[], prompt2=[], prompt3=[], prompt=Prompt}};
                {error, Reason} ->
                    gen_fsm:reply(StateData#state.replyto, {error, Reason}),
        			{stop, normal, StateData}
            end
    end.

waiting(timeout, StateData) -> {stop, timeout, StateData}.
waiting_exec(timeout, StateData) -> {stop, timeout, StateData}.

starting({connect, Host, User, Password}, From, StateData) ->
    case ssh:connect(Host, 22, [{silently_accept_hosts, true}, 
                                 {user_interaction, false}, 
                                 {user_dir, "."}, 
                                 {user, User}, 
                                 {password, Password},
                                 {timeout, ?TIMEOUT}]) of
		{ok, Connection} ->
			{ok, Channel} = ssh_connection:session_channel(Connection, ?TIMEOUT),
			success = ssh_connection:open_pty(Connection, Channel, "dumb", ?TERM_WIDTH, ?TERM_HEIGHT, [], ?TIMEOUT),
			ok = ssh_connection:shell(Connection, Channel),
			{next_state, reading_banner, StateData#state{connection=Connection, channel=Channel, replyto=From}};
		{error, Reason} -> {stop, normal, {error, Reason}, StateData}
    end.

ready({send, Command}, From, StateData) ->
    send_cmd(StateData, Command),
    {next_state, waiting, StateData#state{replyto=From, command=Command}};
ready({exec, Command}, From, StateData) ->
    exec_cmd(StateData, Command),
    
    {next_state, waiting_exec, StateData#state{replyto=From, command=Command}}.

handle_event(close, _StateName, StateData) ->
    {stop, normal, StateData};

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

handle_info(Info, starting, StateData) ->
    ?LOG("starting info: ~p~n", [Info]),
    {next_state, starting, StateData};

handle_info(Info, reading_banner, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info,
    {next_state, reading_banner, StateData, ?BANNER_TIMEOUT};
  
handle_info(Info, guess_prompt1, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    ?LOG("guess prompt1: ~p~n", [Data]),
    {next_state, guess_prompt1, StateData#state{prompt1=[Data | StateData#state.prompt1]}, ?PROMPT_TIMEOUT};

handle_info(Info, guess_prompt2, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    ?LOG("guess prompt2: ~p~n", [Data]),
    {next_state, guess_prompt2, StateData#state{prompt2=[Data | StateData#state.prompt2]}, ?PROMPT_TIMEOUT};

handle_info(Info, guess_prompt3, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    ?LOG("guess prompt3: ~p~n", [Data]),
    {next_state, guess_prompt3, StateData#state{prompt3=[Data | StateData#state.prompt3]}, ?PROMPT_TIMEOUT};

handle_info(Info, ready, StateData) ->
	case Info of
		{ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} ->
			?LOG("ready: ~p~n", [Data]),
			{next_state, guess_prompt3, StateData#state{prompt3=[Data | StateData#state.prompt3]}, ?COMMAND_TIMEOUT};
		I -> 
			?LOG("unmatched: ~p~n", [I]),
			{next_state, ready, StateData, ?COMMAND_TIMEOUT}
	end;

handle_info(Info, waiting, StateData) ->
	{ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,

	LData = binary_to_list(Data),	
	Received = StateData#state.received ++ [Data],
	?LOG("send data: ~p ~n", [Data]),	
	case string:str(LData, StateData#state.prompt) of
		0 ->
			% no prompt wait some more
			{next_state, waiting, StateData#state{received = Received}, ?COMMAND_TIMEOUT};
		_Pos ->			
			gen_fsm:reply(StateData#state.replyto, {ok, Received}),
			{next_state, ready, StateData#state{received = [], command = []}}
	end;

handle_info(Info, waiting_exec, StateData) ->
	io:format("+I: ~p ~n", [Info]),	
	case Info of 
		{ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} ->
			Received = StateData#state.received ++ [Data],
			{next_state, waiting_exec, StateData#state{received = Received}, ?COMMAND_TIMEOUT};
		{ssh_cm, _ConnectionRef, {eof, _ChannelId}} ->
			gen_fsm:reply(StateData#state.replyto, {ok, StateData#state.received}),
			{next_state, ready, StateData#state{received = [], command = []}};
		{ssh_cm, _ConnectionRef, {exit_signal, _ChannelId, Signal, ErrorMsg, _Lang}} ->
			gen_fsm:reply(StateData#state.replyto, {error, Signal, ErrorMsg}),
			{next_state, ready, StateData#state{received = [], command = []}}
	end;
	
handle_info(Info, StateName, StateData) ->
    ?LOG("unknown info: ~p~n", [Info]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, StateData) ->
    %% gproc:unregister_name(StateData#state.id),
    ssh:close(StateData#state.connection),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%% Internal functions

send_cmd(StateData, Cmd) -> 
	io:format("+S: ~p ~p ~n", [StateData, Cmd]),
	ssh_connection:send(StateData#state.connection, StateData#state.channel, Cmd ++ "\n").
send_cr(StateData) -> ssh_connection:send(StateData#state.connection, StateData#state.channel, "\n").
exec_cmd(StateData, Cmd) ->
	{ok, Channel} = ssh_connection:session_channel(StateData#state.connection, ?WINDOW_SIZE, ?PACKET_SIZE, ?TIMEOUT),
	ssh_connection:exec(StateData#state.connection, Channel, Cmd, ?COMMAND_TIMEOUT).

guess_prompt(Prompt1, Prompt2, Prompt3) when Prompt1 == Prompt2, Prompt1 == Prompt3 -> {ok, binary_to_list(Prompt1)};
guess_prompt(_Prompt1, _Prompt2, _Prompt3) -> {error, propmpts_does_not_match}.
