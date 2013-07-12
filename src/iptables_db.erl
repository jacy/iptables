-module(iptables_db).
-behaviour(gen_server).

-export([start_link/0, stop/0,execute/1,execute/2, 
	is_ip_blocked/1, is_user_exist/2, count_block_ips/0,blocked_ips/2,block_ip/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DB, ips_db_pool).
-include("ip.hrl").

%% ==========================Public API======================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

is_ip_blocked(Ipnum) ->
	Result = execute(<<"select 1 from block_ips where number_start <=? and number_end>=?">>,[Ipnum, Ipnum]),
	is_Exist(Result).

is_user_exist(Name, Password) ->
	Result = execute(<<"select status from user where username =? and password =?">>,[Name, utils:md5(Password)]),
	is_Exist(Result).

count_block_ips() ->
	[[Result]] = execute(<<"select count(*) from block_ips">>),
	Result.

blocked_ips(Limit, Offset) ->
	execute(<<"select address_start, address_end from block_ips order by number_start limit ? offset ?">>, [Limit, Offset]).

block_ip(AddStart, AddEnd, NumStart, NumEnd) ->
	insert(<<"insert IGNORE into block_ips(address_start,address_end,number_start,number_end) values(?,?,?,?)">>, [AddStart, AddEnd, NumStart, NumEnd]).
	
%% ==========================Private API======================================
insert(Sql,Arg) ->
	emysql:execute(?DB, Sql, Arg).

execute(Sql) ->
	{ _, _, _, Result, _ } = emysql:execute(?DB, Sql),
	Result.
execute(Sql, Arg) ->
	{ _, _, _, Result, _ } = emysql:execute(?DB, Sql, Arg),
	Result.

is_Exist([[1]]) -> 1;
is_Exist(_) -> 0.
	

%% =========================Callback=========================================
init([]) ->
	process_flag(trap_exit,true),
	crypto:start(),
	application:start(emysql),
	emysql:add_pool(?DB, 1000,
		"erlang", "111111", "localhost", 3306,
		"ips", utf8),
	io:format("********Database is staring*****~n"),
  	{ok, []}.

handle_call(Msg, _From, State) ->
  io:format("********Database receive unknown call Msg:~p*****~n",[Msg]),
  {reply, ok, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Msg, State) -> 
	io:format("********Database receive unknown cast Msg:~p*****~n",[Msg]),
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("********Database receive unknown Msg:~p ~n*****~n",[Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format("********Database connection is stopping*****~n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.