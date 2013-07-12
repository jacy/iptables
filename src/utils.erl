-module(utils).
-compile(export_all).

timestamp_millisecs() ->
	{MegaSecs,Secs,MicroSecs} = now(),
        ((MegaSecs*1000000 + Secs)*1000000 + MicroSecs)/1000. 

nowstring() ->
	{Y,Mon,D} = date(),
	{H,M,S} = time(),
	lists:concat([Y,"-",Mon,"-",D," ",H,":",M,":",S," "]).

uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
    Str = io_lib:format("~8.16.0b_~4.16.0b_4~3.16.0b_~4.16.0b_~12.16.0b",
                        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
	lists:flatten([Str]).

md5(Source) ->
	lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5(Source)]).