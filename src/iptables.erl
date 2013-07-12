%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc iptables.

-module(iptables).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the iptables server.
start() ->
    iptables_deps:ensure(),
    ensure_started(crypto),
    application:start(iptables).


%% @spec stop() -> ok
%% @doc Stop the iptables server.
stop() ->
    application:stop(iptables).
