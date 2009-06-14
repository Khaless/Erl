%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for erl_http.

-module(erl_http_web).
-author('author <author@example.com>').

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
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
		"chat/" ++ Room ->
			Response = Req:ok({"text/html; charset=utf-8",
					  [{"Server",  "Mochiweb-ErlBot"}],
					  chunked}),
			router:login(Room, self()),
			feed(Response, Room);
                _ ->
			Req:not_found()
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

feed(Response, Room) ->
	receive
		{xmpp_msg, Msg} ->
			Html = io_lib:format("~s<br />\n", [Msg]),
			io:format("writing msg ~s\n", [Msg]),
			Response:write_chunk(Html)
	end,
	feed(Response, Room).
		
%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
