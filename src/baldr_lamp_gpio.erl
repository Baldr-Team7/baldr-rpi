-module(baldr_lamp_gpio).

-export([start_link/1]).

start_link({pins, R, G, B}) ->
	{ok, Rpin} = gpio:start_link(R, output),
	{ok, Gpin} = gpio:start_link(G, output),
	{ok, Bpin} = gpio:start_link(B, output),
	Pid = spawn_link(fun() -> serve({pins, Rpin, Gpin, Bpin}) end),
	{ok, Pid}.

serve(Pins) ->
	receive 
		{lamp_set, Pid, Color} -> 
			write(Pins, Color),
			Pid ! {lamp_set_r, self()},
			serve(Pins)
	end.

write(Pins, false) -> 
	write(Pins, {color, 0, 0, 0});
write({pins, Rpid, Gpid, Bpid}, {color, R, G, B}) -> 
	gpio:write(Rpid, colorOneOrZero(R)),
	gpio:write(Gpid, colorOneOrZero(G)),
	gpio:write(Bpid, colorOneOrZero(B)).

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