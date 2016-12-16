-module(baldr_rpi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("Starting baldr_light ~n"),
    
    Config = load_configuration(),
    io:format("Config: ~n~p", [Config]),

    [{discoveryCode, DiscoveryCode}, {home, CHomeId}, {mqtt, Mqtt}, {lights, Lights} | _ ] = Config,

    HomeId = case CHomeId of 
    			undefined -> % if theres no HomeId, enter discovery mode
    				{ok, H} = wait_for_discovery_message(DiscoveryCode, {mqtt, Mqtt}),
    				H;
    			_ -> 		 % else use the existing one
    				CHomeId 
    		end,

    LightPids = lists:map(fun(L) -> {ok, P} = launch_light({light_args, HomeId, Mqtt, L}), P end, Lights),
    save_config(LightPids, HomeId, Config),
    baldr_supervisor:start_link().

launch_light(Args) ->
	{ok, Pid} = gen_server:start_link(baldr_light, Args, []),
	baldr_light:update(Pid),
	{ok, Pid}.

stop(_State) ->
    ok.

save_config(LightPids, HomeId, Config)->
	% GET light state
	States = lists:map(fun(Pid) -> {baldr_light_state, Light, _, _} = baldr_light:get_state(Pid), Light end, LightPids),

	L = [{lights, States}, {home, HomeId}],
	NewConfig = proplists:substitute_aliases(L, Config),

	io:format("New Config ~p~n", [NewConfig]),
	ok.

save_configuration(L) -> 
	Filename = "consfig.txt",
	
	file:write_file(
		Filename, 
		lists:map(
			fun(Term) -> io_lib:format("~tp.~n", [Term]) end, L), 
		[{encoding, utf8}]).

load_configuration() -> 
	{ok, Terms} = file:consult("config.txt"),
	Terms.

wait_for_discovery_message(DiscoveryCode, {mqtt, {Host, Port}}) ->
	{ok, C} = emqttc:start_link([{host, Host}, {port, Port}, {logger, info}, {keepalive, 0}]),
	emqttc:subscribe(C, <<"lightcontrol/discovery">>, qos1),
	receive_discovery_message(DiscoveryCode).

receive_discovery_message(DiscoveryCode) ->
	receive 
		{publish, <<"lightcontrol/discovery">>, Payload} -> 
			io:format("P: ~p~n", [Payload]),
			{Message} = jiffy:decode(Payload),
			case handle_discovery_message(DiscoveryCode, Message) of
				{ok, Home} -> 
					{ok, Home};
				_ -> 
					receive_discovery_message(DiscoveryCode)
			end
	end.

handle_discovery_message(DiscoveryCode, Message) ->
	Protocol  = proplists:get_value(<<"protocolName">>,  Message),
	Version   = proplists:get_value(<<"version">>,   Message),
	
	io:format("M: ~p~n", [Message]),

	{Discovery} = proplists:get_value(<<"discovery">>, Message),
	ReceivedDiscoveryCode = proplists:get_value(<<"discoveryCode">>, Discovery),
	Home = proplists:get_value(<<"home">>, Discovery),

	io:format("D: ~p~n", [{Discovery, DiscoveryCode}]),

	case {Protocol, Version, ReceivedDiscoveryCode} of
		{<<"baldr">>, 1, DiscoveryCode} -> {ok, Home};
		_ -> false
	end.