-module(iptables_views).

-compile(export_all).

-include("ip.hrl").
-define(PARAM_NOT_FOUND, "none").
-define(ANONYMOUS, false).
-define(REQUIRED_LOGIN, true).
-define(HOST, "http://localhost:7060").
-define(SHOW_RULES_URL, ?HOST ++ "/web/showrules").
-define(LOGIN_SUCCESS_URL, ?SHOW_RULES_URL).
-define(KEY_PAGE, "page").
-define(ITEM_PER_PAGE, 15).
-define(GET, 'GET').
-define(POST, 'POST').

urls() -> [
	{"^api/checkip/?$", checkip, ?ANONYMOUS},
	{"^web/login$", login, ?ANONYMOUS},
	{"^web/(.+?)/?$", web, ?REQUIRED_LOGIN}
].

get_param(Key, InputData) ->
    proplists:get_value(Key, InputData, ?PARAM_NOT_FOUND).
	
handle_login(Req, InputData) ->
	Username = get_param("username", InputData),
	Pwd = get_param("password", InputData),
	check_login_param(Req, Username, Pwd).

check_login_param(Req, ?PARAM_NOT_FOUND, _) ->
	html_response(Req, login_dtl, [{errorMsg,"Username is required"}]);
check_login_param(Req, _, ?PARAM_NOT_FOUND) ->
	html_response(Req, login_dtl, [{errorMsg,"Password is required"}]);
check_login_param(Req, Username, Password) ->
	case iptables_db:is_user_exist(Username,Password) of 
		1 ->
			SessionId = utils:uuid(),
			Session = #session{id=SessionId, username=Username},
			session:start_link(Session),
			SessionCookie = iptables_web:set_session_cookie(SessionId),
			UsernameCookie = iptables_web:set_username_cookie(Username),
			redirect(Req, ?LOGIN_SUCCESS_URL, [SessionCookie, UsernameCookie]);
		0 ->
			html_response(Req, login_dtl, [{errorMsg,"Username or password error"}])
	end.
			
	
handle_check(Req, InputData) ->
	QueryIp = get_param("ip", InputData),
 	case ip_to_long:cast(QueryIp) of
        {ok, GeoIP} ->
            io:format("Query Ip is:~p, geoip:~p~n", [QueryIp, GeoIP]),
            response(Req,  [{return_code, 1}, {blocked, iptables_db:is_ip_blocked(GeoIP)}]);
        {error, R} ->
            response_error(Req, R)
    end,
    response(Req, [{success, 1}]).

showrules(_, Req, InputData) ->
	do_showrules(Req, InputData,[]).

do_showrules(Req, InputData, BindData) ->
	Page = proplists:get_value(?KEY_PAGE, InputData, 1),
	Count = iptables_db:count_block_ips(),
	PageCount = case Count of
					0 -> 0;
					C -> trunc(C / ?ITEM_PER_PAGE) + 1
				end,
	Offset = ?ITEM_PER_PAGE * (Page -1),
	Data = case Count > Offset of
		true ->
			iptables_db:blocked_ips(?ITEM_PER_PAGE, Offset);
		false ->
			[]
	end,
	html_response(Req, listrules_dtl, [{count,PageCount},{page,Page},{data,Data}] ++ BindData).

logout(_, Req, _InputData) ->
	SessionId = iptables_web:session_from_cookie(Req),
	session:remove_session(SessionId),
	go_to_login_page(Req).

blockbyip(?GET, Req, _InputData) ->
	do_showrules(Req, [], []);
blockbyip(?POST,Req, InputData) ->
	try 
		AddrStart = get_param("addressStart", InputData),
		check_param(AddrStart,"IP address start is required"),
		NumStart = check_ip(AddrStart,"IP address start is not valid"),
		AddrEnd = get_param("addressEnd", InputData),
		check_param(AddrEnd,"IP address end is required"),
		NumEnd = check_ip(AddrEnd,"IP address start is not valid"),
		check_ips(NumStart, NumEnd),
		iptables_db:block_ip(AddrStart, AddrEnd, NumStart, NumEnd),
		redirect(Req, ?SHOW_RULES_URL, [])
	catch
		throw:ErrorMsg ->
			do_showrules(Req, [], [{errorMsg, ErrorMsg}])
	end.

check_ips(NumStart, NumEnd) ->
	case NumEnd < NumStart of
        true ->
			throw("End of IP address should be bigger than start");
        false ->
            ok
    end.
	
	
check_ip(QueryIp,ErrorMsg) ->
	case ip_to_long:cast(QueryIp) of
        {ok, GeoIp} ->
            GeoIp;
        {error, _R} ->
			throw(ErrorMsg)
    end.

check_param(?PARAM_NOT_FOUND, ErrorMsg) ->
	throw(ErrorMsg);
check_param(_, _) ->
	ok.

go_to_login_page(Req) ->
 	iptables_views:html_response(Req, login_dtl).
	

checkip(?GET, Req) ->
    handle_check(Req, Req:parse_qs());
checkip(?POST, Req) ->
    handle_check(Req, Req:parse_post()).
login(?GET, Req) ->
    handle_login(Req, Req:parse_qs());
login(?POST, Req) ->
    handle_login(Req, Req:parse_post()).

web(Method, Req, Param) ->
    Function = list_to_atom(Param),
	case Method of
		?GET ->
    		iptables_views:Function(?GET,Req, Req:parse_qs());
		?POST ->
    		iptables_views:Function(?POST,Req, Req:parse_post())
	end.


redirect(Req, RedirectUrl, Cooikes) ->
	Req:respond({302, [{"Location", RedirectUrl} | Cooikes],""}).

html_response(Req, DtlTemplate) ->
	html_response(Req, DtlTemplate, []).

html_response(Req, DtlTemplate, Data) ->
	{ok, HTMLOutput} = DtlTemplate:render(Data),
	Req:respond({200, [{"Content-Type", "text/html"}], HTMLOutput}).

response(Req, Content) ->
	Req:respond({200, [{"Content-Type", "application/json"}], mochijson2:encode({struct,Content})}).

response_error(Req, ErrorMsg) ->
	response(Req, [{return_code, 0}, {error_msg, ErrorMsg}]).