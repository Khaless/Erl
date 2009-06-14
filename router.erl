-module(router).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([send/2, login/2, logout/1]).

-define(SERVER, global:whereis_name(?MODULE)).

% Holds mapping between Room and pid 
-record(state, {pid2room, room2pid}).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%
%
%

% Sends message to anyone in Room
send(Room, Msg) ->
	gen_server:call(?SERVER, {send, Room, Msg}).

login(Room, Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {login, Room, Pid}).

logout(Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {logout, Pid}).
%
%
%

init([]) ->
	process_flag(trap_exit, true),
	
	% use ets for routing tables
	{ok, #state{
		pid2room = ets:new(?MODULE, [bag]),
		room2pid = ets:new(?MODULE, [bag])
		}
	}.

handle_call({login, Room, Pid}, _From, State) when is_pid(Pid) ->
	ets:insert(State#state.pid2room, {Pid, Room}),
	ets:insert(State#state.room2pid, {Room, Pid}),
	
	link(Pid), % this will alert us if they exit so we can call logout on the Pid
	
	{reply, ok, State};

handle_call({logout, Pid}, _From, State) when is_pid(Pid) ->
	unlink(Pid),
	PidRows = ets:lookup(State#state.pid2room, Pid),
	case PidRows of
		[] ->
			ok;
		_ ->
			RoomRows = [ {R,P} || {P,R} <- PidRows ], % invert tuples
			% delete all room->pid entries
			ets:delete(State#state.pid2room, Pid),
			% delete all pid->room entries
			[ ets:delete_object(State#state.room2pid, Obj) || Obj <- RoomRows ]
	end,
	{reply, ok, State};

handle_call({send, Room, Msg}, _From, State) ->
	% get PID's who are in this room
	Pids = [ P || {_Room, P} <- ets:lookup(State#state.room2pid, Room) ],
	
	% Send Msg to all these Pids
	M = {xmpp_msg, Msg},
	[ Pid ! M || Pid <- Pids],
	{reply, ok, State}.

handle_info(Info, State) ->
	case Info of 
		{'EXIT', Pid, Why} ->
			% logout PID
			handle_call({logout, Pid}, trash, State);
		Unknown ->
			io:format("Trapped Unhandled message: ~w~n", [Unknown])
	end,
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


	


	
