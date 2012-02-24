-module(ssha_control).

-include("mc.hrl").

-export([add_node/3, del_node/1, reconnect/1, nodes_list/0, send_command/2, ipsec/1]).
-export([test_parse_samgr/0]).

add_node(Host, User, Title) ->	
	Node = #node{ id = User ++ "#" ++ Host, host = Host, user = User, title = Title},
	try 
		persist:add_node(Node),
		connect(Node),
		ok
	catch 
		E -> {error, E}
	end.

del_node(Id) ->
	persist:del_node(Id),
	close(Id),
	ok.

reconnect(Id) -> 
	close(Id),
	Node = persist:node_by_id(Id),
	_Agent = connect(Node),
	ok.

nodes_list() -> 
	NL = persist:nodes_list(),
	[{struct, [{id, list_to_binary(I)}, {host, list_to_binary(H)}, {user, list_to_binary(U)}, {title, list_to_binary(T)}]} || {node, I, H, U, T, _ } <- NL].

send_command(Id, Command) -> 
	R = send(Id, Command),
	lists:foldl(fun(I, A) -> <<A/binary, I/binary>> end, <<>>, R).

ipsec(Id) -> 
	R = send(Id, "sa_mgr show -detail"),
	parse_samgr(R).


%%% Internal functions

connect(Node) ->
	{ok, Agent} = ssha:start(Node#node.id),
	ssha:connect(Agent, Node#node.host, Node#node.user, ""),
	Agent.

close(Id) -> 
	case gproc:lookup_local_name(Id) of
		undefined -> ok;
		Pid -> ssha:close(Pid)
	end.

send(Id, Command) ->
	case gproc:lookup_local_name(Id) of 
		undefined -> 
			Node = persist:node_by_id(Id),
			Pid = connect(Node),
			{ok, R} = ssha:send(Pid, Command),
			R;
		Pid  -> 
			{ok, R} = ssha:send(Pid, Command),
			R
	end.

parse_samgr(R) ->
	[LoType, LoId, LoBlock, A] = lists:foldl(fun(I, [Type, Id, Block, A]) -> 
			case re:run(I, "^(ISAKMP|IPsec) connection id: ([0-9]*).*$", [{capture, all_but_first, binary}]) of 
				{match, [NType, NId]} -> 
					case Type =/= undefine of 
						true -> 
							[NType, NId, [], A ++ [[Type, Id, Block]]];
						false -> 
							[NType, NId, [], A]
					end;
				_ ->
					Inf = re:replace(I, "^[ ]{1,7}sa[ |:]?(limits:|timing:)?[ ]{0,2}", "", [{return,list}]),										
					In = re:replace(Inf, ", remote crypto endpt", ": remote crypto endpt", [{return,list}]),
					Pars = parse_sa_params(re:split(In, ": ")),
					[Type, Id, Block ++ Pars, A]
			end
		end, [undefine, "0", [], []], R),
	case LoType =/= undefine of true -> A ++ [[LoType, LoId, LoBlock]]; false -> A end.

parse_sa_params([K, V | L]) ->
	KL = binary_to_list(K),
	case length(KL)>0 of
		true ->
			NK = re:replace(KL, "^[ |\t]{1,10}", "", [{return,binary}]), 
			NV = esc_sa_value(V),
			[{NK, NV}] ++ parse_sa_params(L);
		false -> parse_sa_params([V|L])
	end;
parse_sa_params([_]) -> [];
parse_sa_params([]) -> [].

esc_sa_value(V) ->
	VL = binary_to_list(V),
	case re:run(VL, "\\\\([0-9a-f]{2})", [global, {capture, all_but_first, list}]) of 
		{match, D} ->			
			lists:foldl(fun([I], Vp) -> 
					Bin = list_to_integer(I, 16),
					re:replace(Vp, "\\\\" ++ I, <<Bin>>, [global, {return, binary}])
				end, V, D);
		_ -> V	
	end.

test_parse_samgr() ->
	{ok, D} = file:read_file("test2"),
	R = re:split(D, "\n"),
	RL = lists:map(fun(I) -> binary_to_list(I) end, R),
	O = parse_samgr(RL),
	io:format("~ts~n", [O]).
