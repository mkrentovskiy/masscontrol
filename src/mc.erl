%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mc.

-module(mc).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.


%% @spec start() -> ok
%% @doc Start the mc server.
start() ->
    mc_deps:ensure(),
    ensure_started(crypto),
    ensure_started(ssh),
    ensure_started(mnesia),
    ensure_started(gproc),
    application:start(mc).


%% @spec stop() -> ok
%% @doc Stop the mc server.
stop() -> application:stop(mc).
