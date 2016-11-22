-module(baldr_message_handler).
-export([start_link/5, set_room_topic/2, update_info/2, light_info_to_baldr_json/1]).

home_topic(H)-> list_to_binary([<<"lightcontrol/home/">>,H]).
room_topic(H, R)-> list_to_binary([home_topic(H), <<"/room/">>, R]).
light_topic(H, L)-> list_to_binary([home_topic(H), <<"/light/">>, L]).
light_info_topic(H, L)-> list_to_binary([light_topic(H,L), <<"/info">>]).
light_command_topic(H, L)-> list_to_binary([light_topic(H,L), <<"/commands">>]).

start_link({host, Host}, {port, Port}, {light_controller, LightPid}, {home_id, HomeID}, {light_id, LightID}) ->
	%% connect to broker
	
	io:format("Host:~p~n Port:~p~n LightPid:~p~n HomeID:~p~n LightId:~p~n", [Host, Port, LightPid, HomeID, LightID]),
	
	RoomTopic  = undefined,
	LightInfoTopic = light_info_topic(HomeID, LightID),
	LightCommandTopic = light_command_topic(HomeID, LightID),
	
	spawn_link(fun () ->
		{ok, C} = emqttc:start_link([{host, Host}, {port, Port}, {client_id, <<"simpleClientasdf">>}]),
		emqttc:subscribe(C, LightCommandTopic, qos1),
		serve({emqtt, C}, {light_controller, LightPid}, {state, HomeID, RoomTopic, LightInfoTopic, LightCommandTopic}) 
	end).

serve({emqtt, C}, {light_controller, LightPid}, {state, Home, RoomTopic, LightInfoTopic, LightCommandTopic}) ->
	%% receive message
	io:format("Waiting for message on emqtt client ~p ~n", [{ {emqtt, C}, {light_controller, LightPid}, {state, Home, RoomTopic, LightInfoTopic, LightCommandTopic}}]),
	receive
		{publish, Topic, Payload} ->
			emqttc:publish(C, <<"baldr-log">>, list_to_binary([ "Received message from ", Topic, " : ", Payload])), 
			io:format("Message Received from ~s: ~s~n", [Topic, Payload]),
			
			{Message} = jiffy:decode(Payload),
			io:format("Message ~p~n", [Message]),
			io:format("~s~n", [ proplists:get_value(<<"protocolName">>, Message)]),
			
			case proplists:get_value(<<"protocolName">>, Message) of
				<<"baldr">> -> handle_baldr_message( LightPid, Message );
				_ -> default
			end,
			
			serve({emqtt, C}, {light_controller, LightPid}, {state, Home, RoomTopic, LightInfoTopic, LightCommandTopic});

		{baldr_mh_set_room_topic, Pid, R} ->
			emqttc:unsubscribe(C, RoomTopic),
			NewRoomTopic = room_topic(Home, R),
			emqttc:subscribe(C, NewRoomTopic, qos1),

			Pid ! {baldr_mh_set_room_topic_r, self()},
			serve({emqtt, C}, {light_controller, LightPid}, {state, Home, NewRoomTopic, LightInfoTopic, LightCommandTopic});

		{baldr_mh_update_info, Pid, Info} ->
			emqttc:publish(C, LightInfoTopic, light_info_to_baldr_json(Info), [{retain, true}]),
			Pid ! {baldr_mh_update_info_r, self()},
			serve({emqtt, C}, {light_controller, LightPid}, {state, Home, RoomTopic, LightInfoTopic, LightCommandTopic});
		
		{baldr_mh_stop, Pid} -> 
			Pid ! {baldr_mh_stop_r, self()}
	end.

handle_baldr_message( _,    [] ) -> default;
handle_baldr_message( LPid, [{<<"lightCommand">>, Command}|_] ) -> executeCommand( LPid, Command );
handle_baldr_message( LPid, [_|T] ) -> handle_baldr_message( LPid, T ).

executeCommand(C, {<<"lightCommand">>, {Commands}}) -> 
	State = proplists:get_value(<<"state">>, Commands, <<"on">>),
	Color = proplists:get_value(<<"color">>, Commands, <<"#FFFFFF">>),
	baldr_light:set(C, [hex_to_color(Color), binary_to_atom(State, utf8)]).

color_to_hex({color, R, G, B}) -> list_to_binary(["#", dec_to_hex(R), dec_to_hex(G), dec_to_hex(B)]).
dec_to_hex(D) -> integer_to_list(D, 16).

hex_to_color(<<"#", R:2/binary, G:2/binary, B:2/binary>>) -> {color, hex_to_dec(R), hex_to_dec(G), hex_to_dec(B)}.
hex_to_dec(H) -> {ok, [V], []} = io_lib:fread("~16u", binary_to_list(H)), V.

% stop listening at any previous rooms and listen to a new one
set_room_topic(Pid, R) -> 
	Pid ! {baldr_mh_set_room_topic, self(), R},
	receive {baldr_mh_set_room_topic_r, Pid} -> ok end.

update_info(Pid, Info) ->
	Pid ! {baldr_mh_update_info, self(), Info},
	receive {baldr_mh_update_info_r, Pid} -> ok end.

light_info_to_baldr_json(LightInfo) ->
	InfoEJSON = {[
		{<<"protocolName">>, <<"baldr">>},
		{<<"version">>, 1},
		{<<"lightInfo">>, 
			{[
				{<<"state">>, list_to_binary(atom_to_list(proplists:get_value(state, LightInfo, null)))},
				{<<"color">>, color_to_hex( proplists:get_value(color, LightInfo, null) ) },
				{<<"room">>, proplists:get_value(room, LightInfo, null)}
			]}
		}
	]},
	io:format("~p~n", [InfoEJSON]),
	jiffy:encode(InfoEJSON).