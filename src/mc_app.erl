%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mc Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mc application.

-module(mc_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mc.
start(_Type, _StartArgs) ->
    mc_deps:ensure(),
    mc_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mc.
stop(_State) ->
    ok.
