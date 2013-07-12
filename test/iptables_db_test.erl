-module(iptables_db_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("ip.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Any function ending with test_ tells EUnit that it should return a list of tests
start_stop_test_() ->
	{
	 	setup,
		fun start/0,
		fun stop/1,
		{	inorder, % inparallel | inorder
			[
				fun() -> all() end
			]
		}
	}.
 
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
	{ok,Pid} = iptables_db:start_link(), 
	Pid.
 
stop(_) ->
	iptables_db:stop().

all() ->
	is_ip_blocked(),
	is_user_exist(),
	count_block_ips(),
	blocked_ips(),
	block_ip().
 
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_ip_blocked() ->
	?assertEqual(0,iptables_db:is_ip_blocked(1)),
	?assertEqual(1,iptables_db:is_ip_blocked(3232235778)).

is_user_exist() ->
	?assertEqual(1,iptables_db:is_user_exist("erlang","1")), 
	?assertEqual(0,iptables_db:is_user_exist("not exists","4561")).

count_block_ips() ->
	?assertEqual(1,iptables_db:count_block_ips()).

blocked_ips() ->
	?assertMatch([[<<"192.168.1.1">>,<<"192.168.1.10">>]],iptables_db:blocked_ips(1,0)).

block_ip() ->
	Address = "228.228.228.228",
	{ok, Ip} = ip_to_long:cast(Address),
	?assertEqual(0,iptables_db:is_ip_blocked(Ip)),
	iptables_db:block_ip(Address, Address, Ip, Ip),
	?assertEqual(1,iptables_db:is_ip_blocked(Ip)),
	iptables_db:block_ip(Address, Address, Ip, Ip),
	?assertEqual(1,iptables_db:is_ip_blocked(Ip)).
	
