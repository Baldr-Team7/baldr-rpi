-module(baldr_led_gpio).

-export([start_link/1, set/2]).

start_link({pins, R, G, B}) ->
	io:format("Starting red~n", []),
	{ok, Rpin} = gpio:start_link(R, output),
	io:format("Starting green~n", []),
	{ok, Gpin} = gpio:start_link(G, output),
	io:format("Starting blue~n", []),
	{ok, Bpin} = gpio:start_link(B, output),
	io:format("Serving~n", []),
	Pid = spawn_link(fun() -> serve({pins, Rpin, Gpin, Bpin}) end),
	{ok, Pid}.

serve(Pins) ->
	io:format("Waiting for gpio_set~n", []), 
	receive 
		{gpio_set, Pid, Color} -> 
			write(Pins, Color),
			Pid ! {gpio_set_r, self()},
			serve(Pins)
	end.

write(Pins, false) -> 
	write(Pins, {color, 0, 0, 0});
write({pins, Rpid, Gpid, Bpid}, {color, R, G, B}) -> 
	io:format("Setting Color ~p ~p ~n", [{color, R, G, B}, {pins, Rpid, Gpid, Bpid}]),
	gpio:write(Rpid, R),
	gpio:write(Gpid, G),
	gpio:write(Bpid, B),
	io:format("Done~n", []).

colorOneOrZero({color, R, G, B}) ->
	if
		R == 0 -> NR = 0;
		true -> NR = 1
	end,
	if
		G == 0 -> NG = 0;
		true -> NG = 1
	end,
	if
		B == 0 -> NB = 0;
		true -> NB = 1
	end,
	{color, NR, NG, NB}.


set(Pid, false) -> 
	set(Pid, {color, 0, 0, 0});
set(Pid, true)  -> 
	set(Pid, {color, 255, 255, 255});
set(Pid, Color) -> 
	io:format("Setting GPIO~n", []),
	Pid ! {gpio_set, self(), colorOneOrZero(Color)},
	receive {gpio_set_r, Pid} -> ok end.
