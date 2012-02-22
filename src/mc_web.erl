%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for mc.

-module(mc_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
					"nodes_list" ->
						HL = ssha_control:nodes_list_json(),
						Req:ok({"application/json", mochijson2:encode(HL)});
					"add_node" ->
						Q = Req:parse_qs(),						
						H = proplists:get_value("h", Q),
						U = proplists:get_value("u", Q),
						T = proplists:get_value("t", Q),
						R = ssha_control:add_node(H, U, T),
						Req:ok({"application/json", mochijson2:encode(R)});
					"del_node" ->
						Q = Req:parse_qs(),						
						I = proplists:get_value("id", Q),
						R = ssha_control:del_node(I),						
						Req:ok({"application/json", mochijson2:encode(R)});
					"reconnect" ->
						Q = Req:parse_qs(),						
						I = proplists:get_value("id", Q),
						R = ssha_control:reconnect(I),						
						Req:ok({"application/json", mochijson2:encode(R)});
					"command" ->
						Q = Req:parse_qs(),						
						I = proplists:get_value("id", Q),
						C = proplists:get_value("c", Q),
						R = ssha_control:send_command(I, C),						
						Req:ok({"application/json", mochijson2:encode(R)});
					"git" ->
						Q = Req:parse_qs(),						
						I = proplists:get_value("id", Q),
						C = proplists:get_value("c", Q),
						R = ssha_control:send_command(I, C),										
						{ok, P} = add_to_git(I, C, R),
						Req:ok({"application/json", mochijson2:encode(P)});
					"ipsec" ->
						Q = Req:parse_qs(),						
						I = proplists:get_value("id", Q),
						R = ssha_control:ipsec(I),										
						Req:ok({"application/json", mochijson2:encode(R)});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

add_to_git(Id, Command, Result) ->
	SPath = mc_deps:local_path(["git"]),
	case filelib:is_dir(SPath) of
		false ->
			file:make_dir(SPath),
			Init = os:cmd("cd git; git init");
		true -> Init = ""
	end,
	Path = mc_deps:local_path(["git", Id]),	
	File = filename:join([Path, Command]),
	case filelib:ensure_dir(Path) of
		ok ->
			file:make_dir(Path),
			file:write_file(File, Result),
			Cmd = "cd git; git add " ++ Id ++ "; git commit " ++ Id ++ " -m 'Commit command result'",
			R = os:cmd(Cmd),
			{ok, Init ++ "\n" ++ R};
		E -> E 
	end.



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
