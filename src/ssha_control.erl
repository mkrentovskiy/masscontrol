-module(ssha_control).

-behaviour(gen_server).

-include("mc.hrl").

-export([start/0, add_node/3, del_node/1, reconnect/1, nodes_list/0, nodes_list_json/0, send_command/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {nodes = [], agents = [] }).

start() -> gen_server:start_link({local, ssha_control}, ?MODULE, [], []).

add_node(Host, User, Title) ->	
	Node = #node{ id = User ++ "." ++ Host, host = Host, user = User, title = Title},
	gen_server:call(ssha_control, {add_node, Node}, 60000).	
del_node(Id) -> gen_server:call(ssha_control, {del_node, Id}).
reconnect(Id) -> gen_server:call(ssha_control, {reconnect, Id}).

nodes_list() -> gen_server:call(ssha_control, nodes_list).
nodes_list_json() -> 
	NL = nodes_list(),	
	[{struct, [{id, list_to_binary(I)}, {host, list_to_binary(H)}, {user, list_to_binary(U)}, {title, list_to_binary(T)}]} || {node, I, H, U, T, _ } <- NL].

send_command(Id, Command) -> gen_server:call(ssha_control, {send_command, Id, Command}, 600000).


init([]) -> 
	Nodes = persist:nodes_list(),
	{ok, #state{ nodes = Nodes, agents = dict:new() }}.

handle_call(nodes_list, _From, State) -> {reply, State#state.nodes, State};

handle_call({add_node, Node}, _From, State) ->
	try 
		persist:add_node(Node),
		Agent = connect(Node#node.host, Node#node.user, ""),
		NAgents = dict:store(Node#node.id, 
							 Agent, 
							 State#state.agents),
		{reply, ok, #state{ nodes = State#state.nodes ++ [Node], agents = NAgents}}
	catch 
		E -> {reply, E, State}	
	end;

handle_call({del_node, Id}, _From, State) ->
	persist:del_node(Id),
	NAgents = case dict:find(Id, State#state.agents) of
		{ok, A} ->
			close(A),
			dict:erase(Id, State#state.agents);				  
		error -> State#state.agents
	end,
	NState = #state{ nodes = [X || X <- State#state.nodes, X#node.id /= Id], agents = NAgents },
	{reply, ok, NState};

handle_call({reconnect, Id}, _From, State) ->
	NAgents = case dict:find(Id, State#state.agents) of
		{ok, A} ->
			[Node] = [X || X <- State#state.nodes, X#node.id == Id],
			dict:update(Id, fun(Old) -> close(Old), connect(Node#node.host, Node#node.user, "") end, State#state.agents);
		error -> State#state.agents
	end,
	{reply, ok, State#state{ agents = NAgents}};


handle_call({send_command, Id, Command}, _From, State) ->
	try 
		case dict:find(Id, State#state.agents) of
			{ok, A} ->
				R = send(A, Command),
				{reply, R, State};
			error -> 
				{NAgents, R} = lists:foldl(fun(I, {A, T}) ->
						case I#node.id == Id of
							true -> 
								APid = connect(I#node.host, I#node.user, ""),
								TR = T ++ send(APid, Command),
								{ dict:store(I#node.id, APid, A), TR };
							false -> {A, T}
						end
					end, {State#state.agents, []} , State#state.nodes), 
				{reply, R, State#state { agents = NAgents }}
		end
	catch 
		E -> {reply, E, State}	
	end.

handle_cast(Msg, State) -> {noreply, State}.
handle_info(Info, State) -> {noreply, State}.
terminate(Reason, State) -> ok.

code_change(OldVsn, State, Extra) -> {ok, State}.

%%% Internal functions

connect(Host, User, Password) ->
	{ok, Agent} = ssha:start(),
	ssha:connect(Agent, Host, User, Password),
	Agent.
	
send(Agent, Command) -> 
	{ok, R} = ssha:send(Agent, Command),
	lists:foldl(fun(I, A) -> A ++ "\n" ++ I end, [], R).

close(Agent) -> ssha:close(Agent).
