-module(baldr_message_handler).

-export([start_link/4]).


home_topic(H)-> list_to_binary([<<"lightcontrol/home/">>,H]).
room_topic(H, R)-> list_to_binary([home_topic(H), <<"/room/">>, R]).
light_topic(H, L)-> list_to_binary([home_topic(H), <<"/light/">>, L]).

start_link({host, Host}, {port, Port}, {home_id, HomeID}, {light_id, LightID}) ->
	%% connect to broker
	{ok, C} = emqttc:start_link([{host, Host}, {port, Port}, {client_id, <<"simpleClient">>}]),
	Room = undefined,

	io:format("Started EMQTTC~n", []),
	
	emqttc:subscribe(C, light_topic(HomeID, LightID), qos0),
	{ok, HW} = baldr_led_gpio:start_link({pins, 27, 17, 22}),

	serve({emqtt, C}, {hw_controller, HW}, HomeID, Room).

serve({emqtt, C}, {hw_controller, HW}, Home, RoomTopic) ->
	%% receive message
	io:format("Waiting for message~n", []),
	receive
		{publish, Topic, Payload} ->
			emqttc:publish(C, <<"baldr-log">>, list_to_binary([ "Received message from ", Topic, " : ", Payload])), 
			io:format("Message Received from ~s: ~s~n", [Topic, Payload]),
			
			{Message} = jiffy:decode(Payload),
			io:format("Message ~p~n", [Message]),
			io:format("~s~n", [ proplists:get_value(<<"protocolName">>, Message)]),
			
			case proplists:get_value(<<"protocolName">>, Message) of
				<<"baldr">> -> executeCommand(proplists:lookup(<<"lightCommand">>, Message), {hw_controller, HW});
				_ -> default
			end,
			
			serve({emqtt, C}, {hw_controller, HW}, Home, RoomTopic);

		{baldr_mh_set_room, Pid, R} ->
			emqttc:unsubscribe(C, RoomTopic),
			New_room_topic = room_topic(R);
			emqttc:subscribe(C, New_room_topic),
			serve({emqtt, C}, {hw_controller, HW}, Home, New_room_topic)

		{baldr_mh_stop, Pid} -> 
			Pid ! {baldr_mh_stop_r, self()}
	end.

executeCommand({<<"lightCommand">>, {Commands}}, {hw_controller, HW}) -> 
	State = proplists:get_value(<<"state">>, Commands, <<"on">>),
	Color = proplists:get_value(<<"color">>, Commands, <<"#FFFFFF">>),

	case State of
		<<"on">> -> baldr_led_gpio:set(HW, colorFromHex(Color));
		<<"off">> -> baldr_led_gpio:set(HW, false)
	end.

colorFromHex(<<"#", R:2/binary, G:2/binary, B:2/binary>>) -> 
	{color, decFromHexBin(R), decFromHexBin(G), decFromHexBin(B)}.
decFromHexBin(H) -> 
	{ok, [V], []} = io_lib:fread("~16u", binary_to_list(H)), V.
% stop listening at any previous rooms and listen to a new one

set_room(Pid, R) -> 
	Pid ! {baldr_mh_set_room, self(), R},
	receive {baldr_mh_set_room_r, Pid} -> ok end.