%% Usage:
%% {ok, session} = echo_client:start().
%% echo_client:stop(Session).

-module(erl_bot).

-define(MUC, <<"http://jabber.org/protocol/muc">>).
-define(HOST, "tsukasa.net.au").
-define(MUC_HOST, "conference.tsukasa.net.au").
-define(PORT, 5222).
-define(USERNAME, "Erl").
-define(PASSWORD, "IamAfish").
-define(ROOM, "people").

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, stop/1]).
-export([init/0]).

start() ->
    spawn(?MODULE, init, []).

stop(ErlBotPid) ->
    ErlBotPid ! stop.

init() ->
    application:start(exmpp),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    %% Create XMPP ID (Session Key):
    MyJID = exmpp_jid:make(?USERNAME, ?HOST, random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, ?PASSWORD),
    %% Connect in standard TCP:
    _StreamId = exmpp_session:connect_TCP(MySession, ?HOST, ?PORT),
    session(MySession, MyJID).

%% We are connected. We now log in (and try registering if authentication fails)
session(MySession, _MyJID) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    %% Try creating a new user:
	    io:format("Register~n",[]),
	    %% In a real life client, we should trap error case here
	    %% and print the correct message.
	    exmpp_session:register_account(MySession, "m00by"),
	    %% After registration, retry to login:
	    exmpp_session:login(MySession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "Erl is on-line and awaiting your command.")),
    join_room(MySession, ?ROOM),
    loop(MySession).

%% Process exmpp packet:
loop(MySession) ->
    receive
        stop ->
            exmpp_session:stop(MySession);
        %% If we receive a message of type "groupchat"
        Record = #received_packet{packet_type=message, type_attr="groupchat", raw_packet=Packet} ->
	    handle_group_message(Record),
            loop(MySession);
        %% If we receive any other kind of message message we...
        Record = #received_packet{packet_type=message, raw_packet=Packet} ->
	    handle_message(Record),
            loop(MySession);
	%% And any other type of record
        Record ->
	    handle_other(Record),
            loop(MySession)
    end.

handle_group_message(Record) ->
	io:format("~p~n", [Record#received_packet.from]),
	% Extract the Room, Who said it (From) and the Message (Body) from the packet.
	[Room, Rest] = string:tokens(Record#received_packet.from, "@"),
	%[_, From] = string:tokens(Rest, "/"),
	From = "Anon",
	Body = exmpp_message:get_body(Record#received_packet.raw_packet),
	
	io:format("[~s/~s] ~s~n", [Room, From, Body]),
	router:send(Room, io_lib:format("[~s] ~s\n", [From, Body])).

handle_message(Record) ->
	ok.
	%io:format("Got Message~n").

handle_other(Record) ->
	ok.
	%io:format("Got Other Message:~n~p~n", [Record]).

join_room (Session, Room) ->
	exmpp_session:send_packet(Session, #xmlel {name=presence, attrs=[#xmlattr{name=to,
		value=list_to_binary(Room++"@"++?MUC_HOST++"/"++?USERNAME)}],
		children=[#xmlel{name=x, attrs=[#xmlattr{name=xmlns, value=?MUC}]}]}).
