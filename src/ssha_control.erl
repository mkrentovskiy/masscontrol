-module(ssha_control).

-behaviour(gen_server).

-include("mc.hrl").

-export([start/0, add_node/4, del_node/1, nodes_list/0, nodes_list_json/0, send_command/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {nodes = []}).

start() -> gen_server:start_link({local, ssha_control}, ?MODULE, [], []).

add_node(Host, User, Password, Type) ->	
	Node = #node{ id = User ++ "." ++ Host, host = Host, user = User, password = Password, type = Type},
	gen_server:call(ssha_control, {add_node, Node}).	
del_node(Id) -> gen_server:call(ssha_control, {del_node, Id}).

nodes_list() -> gen_server:call(ssha_control, nodes_list).
nodes_list_json() -> 
	NL = nodes_list(),
	[{struct, [{id, list_to_binary(I)}, {host, list_to_binary(H)}, {user, list_to_binary(U)}, {type, T}]} || {node, I, H, U, _, T, _ } <- NL].

send_command(Id, Command) -> gen_server:call(ssha_control, {send_command, Id, Command}, 60000).

init([]) -> 
	Nodes = persist:nodes_list(),
	{ok, #state{ nodes = Nodes }}.

handle_call(nodes_list, _From, State) -> {reply, State#state.nodes, State};

handle_call({add_node, Node}, _From, State) ->
	try 
		persist:add_node(Node),
		{reply, ok, #state{ nodes = State#state.nodes ++ [Node]}}
	catch 
		E -> {reply, E, State}	
	end;

handle_call({del_node, Id}, _From, State) ->
	persist:del_node(Id),
	NState = #state{ nodes = [X || X <- State#state.nodes, X#node.id /= Id] },
	{reply, ok, NState};

handle_call({send_command, Id, Command}, _From, State) ->
	try 		
		R = lists:foldl(fun(I, A) ->  
				case I#node.id == Id of 
					true -> A ++ send(I#node.host, I#node.user, I#node.password, Command);
					false -> A
				end
			end, [], State#state.nodes), 
		{reply, R, State}
	catch 
		E -> {reply, E, State}	
	end.

handle_cast(Msg, State) -> {noreply, State}.
handle_info(Info, State) -> {noreply, State}.
terminate(Reason, State) -> ok.

code_change(OldVsn, State, Extra) -> {ok, State}.

%%% Internal functions

send(Host, User, Password, Command) -> 
	{ok, Agent} = ssha:start(),
	ssha:connect(Agent, Host, User, Password),
	{ok, R} = ssha:send(Agent, Command),
	ssha:close(Agent),
	lists:flatten(R).