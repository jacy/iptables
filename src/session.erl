-module(session).
-behaviour(gen_server).

%% API
-export([start_link/1, current_username/1, remove_session/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

-include("ip.hrl").
-define(USER_NOT_LOGIN, user_not_login).
-define(SESSION_TIME_OUT, 30*60*1000). % 30 minutes
%%====================================================================
%% API
%%====================================================================
start_link(#session{id=Id} = Session) ->
  {ok, Pid} = gen_server:start({global, list_to_atom(Id)}, ?MODULE, [Session], []),
  setup_timer(Pid),
  Pid.


current_username(SessionId) ->
	case pid_of(SessionId) of
		undefined ->
			?USER_NOT_LOGIN;
		Pid ->
			case gen_server:call(Pid, {get, SessionId}) of
				{true, Username} ->
					Username;
				_ ->
					?USER_NOT_LOGIN
			end
	end.
	
remove_session(SessionId) ->
	case pid_of(SessionId) of
		undefined ->
			ok;
		Pid ->
			gen_server:call(Pid, {remove})
	end.
%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Session]) ->
	process_flag(trap_exit,true),
	{ok, Session}.

handle_call({get, SessionId}, _From, State) ->
	Reply = SessionId =:= State#session.id,
	{reply, {Reply, State#session.username}, State, ?SESSION_TIME_OUT};
handle_call({remove}, _From, State) ->
	{stop, normal, ok, State}.

handle_cast({setup_timer}, State) ->
	{noreply, State, ?SESSION_TIME_OUT}.

handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	io:format(">>>>>handle info:~p, with State:~p~n",[_Info, State]),
	{noreply, State}.

terminate(Reason, State) ->
	io:format("******Terminated session:~p, reason:~p ~n",[State#session.id, Reason]),
 	ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
pid_of(GlobalName) ->
	global:whereis_name(list_to_atom(GlobalName)).
	
setup_timer(Pid) ->
	gen_server:cast(Pid, {setup_timer}).