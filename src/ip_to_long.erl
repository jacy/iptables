-module(ip_to_long).
-export([cast/1]).
-define(ERROR_IP,{error, <<"Error ip format">>}).

cast(Addr) when is_list(Addr) ->
    case ip2long(Addr) of
	{ok, Ip} ->    
	    {ok, Ip};
	Error ->
	    Error
    end;
cast(Addr) when is_integer(Addr)-> Addr;
cast(_) -> ?ERROR_IP.

%% @spec ip2long(Address) -> {ok, integer()}
%% @doc Convert an IP address from a string, IPv4 tuple or IPv6 tuple to the
%%      big endian integer representation.
ip2long(Address) when is_integer(Address) ->
    {ok, Address};
ip2long(Address) when is_list(Address) ->
    case catch address_fast(Address, 0, 24) of
	N when is_integer(N) ->
	    {ok, N};
	_ ->
	    case inet_parse:address(Address) of
		{ok, Tuple} ->
		    ip2long(Tuple);
		_Error ->
		    ?ERROR_IP
	    end
    end;
ip2long({B3, B2, B1, B0}) ->
    {ok, (B3 bsl 24) bor (B2 bsl 16) bor (B1 bsl 8) bor B0};
ip2long({W7, W6, W5, W4, W3, W2, W1, W0}) ->
    {ok, (W7 bsl 112) bor (W6 bsl 96) bor (W5 bsl 80) bor (W4 bsl 64) bor
	(W3 bsl 48) bor (W2 bsl 32) bor (W1 bsl 16) bor W0};
ip2long(_) ->
    {error, badmatch}.

address_fast([N2, N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N2, N1, N0]) of
	N when N =< 255 ->
	    address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N1, N0]) of
	N when N =< 255 ->
	    address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case N0 - $0 of
	N when N =< 255 ->
	    address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast(L=[_N2, _N1, _N0], Num, 0) ->
    case list_to_integer(L) of
	N when N =< 255 ->
	    Num bor N
    end;
address_fast(L=[_N1, _N0], Num, 0) ->
    case list_to_integer(L) of
	N when N =< 255 ->
	    Num bor N
    end;
address_fast([N0], Num, 0) ->
    case N0 - $0 of
	N when N =< 255 ->
	    Num bor N
    end.