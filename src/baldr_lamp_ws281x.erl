-module(baldr_lamp_ws281x).
-export([start_link/1]).

start_link(Led) ->
	ws281x_nif:start(),
	Pid = spawn_link(fun() -> serve(Led) end),
	{ok, Pid}.

serve({led, Led}) -> 
	receive 
		{lamp_set, Pid, Color} -> 
			io:format("PWM set to ~p~n", [color_to_int(Color)]),
			ws281x_nif:ws281x_nif(Led, color_to_int(Color)),
			Pid ! {lamp_set_r, self()},
			serve({led, Led})
	end. 

color_to_int({color, R, G, B})-> R * 256 * 256 + G * 256 + B.