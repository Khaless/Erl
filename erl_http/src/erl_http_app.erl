%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erl_http application.

-module(erl_http_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erl_http.
start(_Type, _StartArgs) ->
    erl_http_deps:ensure(),
    erl_http_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erl_http.
stop(_State) ->
    ok.
