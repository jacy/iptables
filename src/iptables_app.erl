%% @author Mochi Media <dev@mochimedia.com>
%% @copyright iptables Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the iptables application.

-module(iptables_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for iptables.
start(_Type, _StartArgs) ->
    iptables_deps:ensure(),
    iptables_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for iptables.
stop(_State) ->
    ok.
