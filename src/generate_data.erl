-module(generate_data).
-export([generate/0]).
%% read data from csv and generate data into liquibase from
-record(country,{id,code,name}).
-record(ip,{id,addrStart,addrEnd,numStart,numEnd,countryId}).
-record(lineState,{countryWriter,ipWriter,lineNumber, countrys, countryId}).

generate() ->
	{ok, CountryWriter} = file:open("county.xml", [read,write, raw, binary]),
	{ok, IPWriter} = file:open("ips.xml", [read,write, raw, binary]),
	for_each_line_in_file("/home/jacy/Desktop/GeoIPCountryWhois.csv",
				fun extract_ip/2, 
				#lineState{lineNumber = 1, countrys = dict:new(), ipWriter = IPWriter,countryWriter=CountryWriter,countryId= 1}),
	file:close(CountryWriter),
	file:close(IPWriter).
	
%% Internal functions
for_each_line_in_file(Name, Proc, Accum) ->
    {ok, Device} = file:open(Name, [read]),
    for_each_line(Device, Proc, Accum).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> 
			file:close(Device), 
			Accum;
        Line -> 
			NewAccum = Proc(Line, Accum),
            for_each_line(Device, Proc, NewAccum)
    end.


extract_ip(Line, #lineState{lineNumber = LineNumber, countrys = Countrys} = State) ->
	 [AddrStart, _, AddrEnd, _, NumStart, _, NumEnd, _, Code, _, Name | _Tail] = string:tokens(Line,"\""), 
	 IpRecord = #ip{id= LineNumber,addrStart =AddrStart,addrEnd=AddrEnd,numStart=NumStart,numEnd=NumEnd},
	 case dict:find(Code, Countrys) of
		{ok, Id} ->
			write_file(State#lineState.ipWriter, create_ip(IpRecord#ip{countryId=Id})),
			State#lineState{lineNumber = LineNumber + 1};
		_ ->
			Id = State#lineState.countryId,
			write_file(State#lineState.countryWriter,create_country(#country{id=Id,code=Code,name=Name})),
			write_file(State#lineState.ipWriter, create_ip(IpRecord#ip{countryId=Id})),
			State#lineState{lineNumber = LineNumber + 1,
				 	countryId = Id + 1,
					countrys=dict:store(Code, Id, Countrys)}
	end.

write_file(Device, Binary) ->
	case file:write(Device, [Binary]) of
		ok ->
			ok;
		{error, Reason} ->
			io:format("Write file error:~p~n",[Reason])
	end.

create_ip(#ip{id= Id,addrStart =AddrStart,addrEnd=AddrEnd,numStart=NumStart,numEnd=NumEnd,countryId=CountryId}) -> 
	SQL = io_lib:format("insert into ip(id,address_start,address_end,number_start,number_end,country_id) values(~p,'~s','~s',~s,~s,~p); ~n",
					[Id,AddrStart,AddrEnd,NumStart,NumEnd,CountryId]),
	list_to_binary(SQL).

create_country(#country{id=Id, code=Code, name=Name}) -> 
	SQL = io_lib:format("insert into country(id, code, name) values(~p,'~s','~s'); ~n", [Id,Code,re:replace(Name,"'","\\\\'",[global])]),
	list_to_binary(SQL).
	
	