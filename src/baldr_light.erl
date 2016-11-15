-module(baldr_light).
-export([ start/0, stop/1, set/2]).

start() ->
	Args = [],
	MqttHost = proplists:get_value(mqtt_host, Args, "tann.si"),
	MqttPort = proplists:get_value(mqtt_port, Args, 8883),

	HomeID = proplists:get_value(home_id, Args, "asdf"),
	LightID = proplists:get_value(light_id, Args, "1f3eaa55-2117-415b-a1fd-607c319473ff"),

	MsgPid = baldr_message_handler:start_link({host, MqttHost}, {port, MqttPort}, {home_id, HomeID}, {light_id, LightID}),
	LedPid = baldr_led_gpio:start_link(),
	
	State = off,
	Color = proplists:get_value(color, Args, {color, 255, 255, 255}),
	Room = undefined,


	Info = {
		{color, Color},
		{room, Room},
		{state, State},
		{id, LightID}
	},

	serve(MsgPid, LedPid, Info),	
	ok.

serve(MsgPid, LedPid, Info) ->
	receive
		{baldr_light_set, Pid, SArgs} ->
			% Get color if set else use old
			Color = prop_get_fallback(color, SArgs, Info),
			State = prop_get_fallback(state, SArgs, Info),
			LightId = proplists:get_value(light_id, Info),

			case State of
				off 	-> baldr_led_gpio:set(false);
				on  	-> baldr_led_gpio:set(Color);
				_ 		-> ok
			end,

			SRoom = proplists:get_value(room, SArgs),
			LRoom = proplists:get_value(room, Info),

			case SRoom == LRoom of
				false -> baldr_message_handler:set_room(SRoom);
				true -> void
			end,


			Info = {
					{color, Color},
					{room, SRoom},
					{state, State},
					{id, LightId}
				},

			% Respond
			Pid ! {baldr_light_set_r, self()},
			serve(MsgPid, LedPid, Info);

		{baldr_light_stop, Pid} -> 
			Pid ! {baldr_light_stop_r, self()}

	end.

change_room(R) -> baldr_message_handler:set_room(R).

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