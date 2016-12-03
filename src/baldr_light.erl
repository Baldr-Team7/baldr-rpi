-module(baldr_light).
-export([ start/0, set/2]).

start() ->
	Args 	 = [],
	MqttHost = proplists:get_value(mqtt_host, Args, "tann.si"),
	MqttPort = proplists:get_value(mqtt_port, Args, 8883),

	HomeID 	= proplists:get_value(home_id, Args, "asdf"),
	LightID = proplists:get_value(light_id, Args, "3"),
	MsgPid 	= baldr_message_handler:start_link({host, MqttHost}, {port, MqttPort}, {light_controller, self()}, {home_id, HomeID}, {light_id, LightID}),
	
	LampMode = ws281x,

	case LampMode of
		text -> 
			io:format("NOTE: Running lamp in text mode"), 
			{ok, LedPid} = baldr_lamp:start_link({type, text});
		gpio -> 
			io:format("NOTE: Running lamp in gpio mode"),
			{ok, LedPid} = baldr_lamp:start_link({type, gpio}, [{pins, 17, 22, 27}]);
		ws281x ->
			io:format("NOTE: Running lamp in pwm mode"),
			{ok, LedPid} = baldr_lamp:start_link({type, ws281x}, [{led, 0}])
	end,

	State 	= off,
	Color 	= proplists:get_value(color, Args, {color, 255, 255, 255}),
	Room 	= undefined,

	LightState 	= [
		{color, Color},
		{room, Room},
		{state, State},
		{id, LightID}
	],

	serve(MsgPid, LedPid, LightState).

serve(MsgPid, LedPid, LightState) ->
	receive
		{baldr_light_set, Pid, SetArgs} ->

			{ColorChanged, Color} = prop_get_fallback(color, SetArgs, LightState),
			{StateChanged, State} = prop_get_fallback(state, SetArgs, LightState),

			case (ColorChanged or StateChanged) of 
				true ->
					case State of
							off -> baldr_lamp:set(LedPid, false), off;
							on  -> baldr_lamp:set(LedPid, Color), off;
							_   -> void
					end;
				_ -> void
			end,
			{RoomChanged, Room}   = prop_get_fallback(room, SetArgs, LightState),

			case RoomChanged of
				true -> baldr_message_handler:set_room_topic(MsgPid, Room);
				_ -> void
			end,

			NewState = [
					{color, Color},
					{room, Room},
					{state, State},
					{id, proplists:get_value(id, LightState)}
				],

			baldr_message_handler:update_info(MsgPid, NewState),

			Pid ! {baldr_light_set_r, self()},
			serve(MsgPid, LedPid, NewState);

		{baldr_light_stop, Pid} -> Pid ! {baldr_light_stop_r, self()};
		M -> io:format("Unhandled Message ~p~n", [M]), serve(MsgPid, LedPid, LightState)
	end.

prop_get_fallback(K, L, F) -> 
	case proplists:get_value(K, L) of 
		undefined 	-> {false, proplists:get_value(K, F)};
		V 			-> {true, V}
	end.
			
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

save_configuration(L) -> 
	Filename = "config.txt",
	
	file:write_file(
		Filename, 
		lists:map(
			fun(Term) -> 
				io_lib:format("~tp.~n", [Term]) 
			end, L), 
		[{encoding, utf8}]).

load_configuration() -> 
	{ok, Terms} = file:consult("config.txt"),
	Terms.