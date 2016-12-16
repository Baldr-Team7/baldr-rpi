-module(baldr_light).
-behaviour(gen_server).
-export([init/1, handle_call/3, set/2, get_state/1, update/1]).

-record(lamp, {type, args, pid}).
-record(light, {id, name, state=off, color={color, 255, 255, 255}, room, lamp}).
-record(light_args, {home, mqtt, light}).
-record(baldr_light_state, {light, led_pid, msg_pid}).

init(Args) ->
	HomeId = Args#light_args.home,
	{MqttHost, MqttPort} = Args#light_args.mqtt,
	Light = Args#light_args.light,

	MsgPid 	= baldr_message_handler:start_link({host, MqttHost}, {port, MqttPort}, {light_controller, self()}, {home_id, HomeId}, {light_id, Light#light.id}),
	
	Lamp = Light#light.lamp,

	{ok, LedPid} = baldr_lamp:start_link({type, Lamp#lamp.type}, Lamp#lamp.args),
	InitializedLamp = Lamp#lamp{pid=LedPid},

	{ok, #baldr_light_state{
		light=Light#light{lamp=InitializedLamp},
		led_pid=LedPid,
		msg_pid=MsgPid
	}}.

handle_call({baldr_light_set, SetArgs}, _, State) ->
			NewState = handle_args(SetArgs, State),
			io:format("NEWSTATE = ~p~n", [NewState]),
			update_lamp(NewState),
			baldr_message_handler:update_info(NewState#baldr_light_state.msg_pid, NewState#baldr_light_state.light),
			{reply, ok, NewState};

handle_call(baldr_light_get_state, _, State) -> {reply, State, State};

handle_call(baldr_light_update, _, State) -> 
	baldr_message_handler:update_info(State#baldr_light_state.msg_pid, State#baldr_light_state.light),	
	{reply, ok, State}.

handle_args([H|T], State) -> handle_args(T, handle_args(H, State));
handle_args([   ], State) -> State;

handle_args({color, C}, State) -> 
	NewLight = State#baldr_light_state.light#light{color=C},
	State#baldr_light_state{light=NewLight};
handle_args({state, S}, State) -> 
	NewLight = State#baldr_light_state.light#light{state=S},
	State#baldr_light_state{light=NewLight};
handle_args({name,  N}, State) -> 
	NewLight = State#baldr_light_state.light#light{name=N},
	State#baldr_light_state{light=NewLight};
handle_args({room,  R}, State) -> 
	baldr_message_handler:set_room_topic(State#baldr_light_state.msg_pid, R),
	NewLight = State#baldr_light_state.light#light{room=R},
	State#baldr_light_state{light=NewLight}.

update_lamp(State) ->
	Light = State#baldr_light_state.light,
	LampPid = State#baldr_light_state.led_pid,
	case Light#light.state of
		on  -> baldr_lamp:set(LampPid, Light#light.color);
		off -> baldr_lamp:set(LampPid, false)
	end.

get_state(C)-> gen_server:call(C, baldr_light_get_state).
set(C, Args) -> gen_server:call(C, {baldr_light_set, Args}).
update(C) -> gen_server:call(C, baldr_light_update).