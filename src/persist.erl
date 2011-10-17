-module(persist).

-include("mc.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([init/0, add_node/1, del_node/1, nodes_list/0]).

init() -> 
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(node, 
		[{disc_copies, [node()]}, 
		{attributes, record_info(fields, node)}]).

add_node(Node) -> 
	mnesia:transaction(fun() -> mnesia:write(Node) end).

del_node(Id) -> 
	mnesia:transaction(fun() -> mnesia:delete({node, Id}) end).

nodes_list() -> 
	case mnesia:transaction(fun() -> qlc:e(qlc:q([X || X <- mnesia:table(node)])) end) of
		{atomic, List} -> List;
		_ -> []
	end.

