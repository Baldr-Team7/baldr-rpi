-module(baldr_message_handler).

-export([start/0]).

start() ->
	%% connect to broker
	{ok, C} = emqttc:start_link([{host, "tann.si"}, {port, 8883}, {client_id, <<"simpleClient">>}]),
	io:format("Started EMQTTC~n", []),
	emqttc:subscribe(C, <<"home/FF/Light/FF">>, qos0),
	io:format("Subscribed to baldr-test~n", []),
	{ok, HW} = baldr_led_gpio:start_link({pins, 27, 17, 22}),
	io:format("Started baldr_led_gpio~n", []),
	io:format("Serving~n", []),
	serve({emqtt, C}, {hw_controller, HW}).

serve({emqtt, C}, {hw_controller, HW}) ->
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
			
			serve({emqtt, C}, {hw_controller, HW});
		{baldr_mh_stop, Pid} -> 
			Pid ! {baldr_mh_stop_r, self()}
	end.

executeCommand({<<"lightCommand">>, {Commands}}, {hw_controller, HW}) -> 
	State = proplists:get_value(<<"state">>, Commands, <<"on">>),
	Color = proplists:get_value(<<"color">>, Commands, <<"#FFFFFF">>),

	BState = case State of
		<<"on">> -> baldr_led_gpio:set(HW, colorFromHex(Color));
		<<"off">> -> baldr_led_gpio:set(HW, false)
	end.

colorFromHex(<<"#", R:2/binary, G:2/binary, B:2/binary>>) -> 
	{color, decFromHexBin(R), decFromHexBin(G), decFromHexBin(B)}.
decFromHexBin(H) -> 
	{ok, [V], []} = io_lib:fread("~16u", binary_to_list(H)), 
	V.