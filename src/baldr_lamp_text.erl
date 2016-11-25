-module(baldr_lamp_text).

-export([start_link/0]).

start_link() ->
	Pid = spawn_link(fun() -> serve() end),
	{ok, Pid}.

serve() -> 
	receive 
		{lamp_set, Pid, Color} -> 
			io:format("Color set to ~p~n", [Color]),
			Pid ! {lamp_set_r, self()},
			serve()
	end. 