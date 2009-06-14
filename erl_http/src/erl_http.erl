%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(erl_http).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the erl_http server.
start() ->
    erl_http_deps:ensure(),
    ensure_started(crypto),
    application:start(erl_http).

%% @spec stop() -> ok
%% @doc Stop the erl_http server.
stop() ->
    Res = application:stop(erl_http),
    application:stop(crypto),
    Res.
