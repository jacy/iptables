%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for iptables.

-module(iptables_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([set_session_cookie/1,set_username_cookie/1,session_from_cookie/1]).
-export([start/1, stop/0, loop/2]).
-define(SESSION_KEY,"SID").
-define(USERNAME_KEY,"UNAME").
-define(SESSION_NOT_FOUND, not_found).

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
    try
		case Path of 
			""->
				handle_login(Req),
				iptables_views:showrules([],Req, []);
			"static/" ++ _  ->
				io:format("Static File:~p~n",[Path]),
				Req:serve_file(Path, DocRoot);
			 _Others ->
				case dispatch(Req, iptables_views:urls()) of
		            none -> 
		            	Req:not_found();
		            Response -> 
		                Response
			    end
		end
    catch
        _Type:undef ->
        	io:format("Url not found:~p~n",[Path]),
			Req:not_found();
        Type:What ->
        	log_error(Path, Type, What),
			iptables_views:response_error(Req, <<"Internal server error">>)
    end.

log_error(Path,Type,What) ->
	 Report = ["web request failed",
              {path, Path},
              {type, Type}, {what, What},
              {trace, erlang:get_stacktrace()}],
    error_logger:error_report(Report).

%% Internal API
handle_login(Req) ->
	case session_from_cookie(Req) of
		?SESSION_NOT_FOUND ->
			iptables_views:go_to_login_page(Req);
		SessionId ->
			io:format("~p Session id:~p~n",[utils:nowstring(), SessionId]),
			case  session:current_username(SessionId) of 
				user_not_login -> 
					iptables_views:go_to_login_page(Req);
				Username ->
					Username
			end
			
	end.
	
get_cookie_value(Req, Key, Default) ->
    case Req:get_cookie_value(Key) of
        undefined -> Default;
        Value -> Value
    end.

session_from_cookie(Req) ->
	get_cookie_value(Req, ?SESSION_KEY, ?SESSION_NOT_FOUND).

set_session_cookie(SessionId) ->
    set_cookie(?SESSION_KEY, SessionId).

set_username_cookie(Username) ->
    set_cookie(?USERNAME_KEY, Username).

set_cookie(Key, Value) ->
    mochiweb_cookies:cookie(Key, Value, [{path, "/"}]).

dispatch(_, []) -> none;
dispatch(Req, [{Regexp, Function, NeedLogin}|T]) -> 
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    Match = re:run(Path, Regexp, [global, {capture, all_but_first, list}]),
    case Match of
        {match,[MatchList]} -> 
			Username = case NeedLogin of
			 true ->
				 handle_login(Req);
			  false ->
				  ""
			end,
            case length(MatchList) of
                0 -> 
                    iptables_views:Function(Method, Req);
                Length when Length > 0 -> 
					Username = handle_login(Req),
                    % We pass URL parameters we captured to the function
                    Args = lists:append([[Method, Req], MatchList]),
                    apply(iptables_views, Function, Args)
            end;
        _ -> 
            dispatch(Req, T)
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.