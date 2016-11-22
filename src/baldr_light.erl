-module(baldr_light).
-export([ start/0, stop/1, set/2, hey/0,test_handler/0]).

hey() -> hey.

test_handler() ->
receive
	_ -> heyy, test_handler()
end.

start() ->
	Args 	 = [],
	MqttHost = proplists:get_value(mqtt_host, Args, "tann.si"),
	MqttPort = proplists:get_value(mqtt_port, Args, 8883),

	HomeID 	= proplists:get_value(home_id, Args, "asdf"),
	LightID = proplists:get_value(light_id, Args, "1"),
	MsgPid 	= baldr_message_handler:start_link({host, MqttHost}, {port, MqttPort}, {light_controller, self()}, {home_id, HomeID}, {light_id, LightID}),
	
	LampMode = text,

	case LampMode of
		text -> 
			io:format("NOTE: Running lamp in text mode"), 
			LedPid = baldr_lamp:start_link({type, text});
		gpio -> 
			io:format("NOTE: Running lamp in gpio mode"),
			LedPid = baldr_lamp:start_link({type, gpio}, [{pins, 17, 22, 27}])
	end,

	State 	= off,
	Color 	= proplists:get_value(color, Args, {color, 255, 255, 255}),
	Room 	= undefined,

	LightState 	= [
		{color, Color},
		{room, Room},
		{state, State},
		{light_id, LightID}
	],

	LightPid = spawn_link(fun()->serve(MsgPid, LedPid, LightState) end),
	baldr_message_handler:update_info(MsgPid, LightState),
	LightPid.

serve(MsgPid, LedPid, LightState) ->
	receive
		{baldr_light_set, Pid, SetArgs} ->
			% Get color if set else use old
			Color = prop_get_fallback(color, SetArgs, LightState),
			State = prop_get_fallback(state, SetArgs, LightState),
			LightId = proplists:get_value(light_id, LightState),

			case State of
				off 	-> baldr_bulb:set(false);
				on  	-> baldr_bulb:set(Color);
				_ 		-> ok
			end,

			SRoom = proplists:get_value(room, SetArgs),
			LRoom = proplists:get_value(room, LightState),

			case SRoom == LRoom of
				false -> baldr_message_handler:set_room(MsgPid, SRoom);
				true -> void
			end,


			LightState = [
					{color, Color},
					{room, SRoom},
					{state, State},
					{id, LightId}
				],

			baldr_message_handler:update_info(MsgPid, LightState),

			% Respond
			Pid ! {baldr_light_set_r, self()},
			serve(MsgPid, LedPid, LightState);

		{baldr_light_stop, Pid} -> 
			Pid ! {baldr_light_stop_r, self()}

	end.

change_room(MsgPid, R) -> baldr_message_handler:set_room(MsgPid, R).

prop_get_fallback(K, L, F) -> proplists:get_value(K, L, proplists:get_value(K, F)).
			
replaceProp({K, V}, L) -> replaceProp(K, V, L).

replaceProp(K, undefined, L) -> L;
replaceProp(K, V, L) -> [ {K, V} | proplists:delete(K, L)].

replaceProps([], L) -> L;
replaceProps([{K, V} | T], L) -> replaceProps(T, replaceProp(K, V, L)).

stop(Pid) -> 
	Pid ! {baldr_light_stop, self()},
	receive {baldr_light_stop_r, Pid} -> ok end.

set(Pid, Args) ->
	Pid ! {baldr_light_set, self(), Args},
	receive	{baldr_light_set_r, Pid} -> ok end.