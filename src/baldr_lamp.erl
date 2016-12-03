-module(baldr_lamp).

-export([start_link/1, start_link/2, set/2]).

start_link( Type ) -> start_link(Type, []).

start_link( {type, text}, _) 		    -> baldr_lamp_text:start_link();
start_link( {type, gpio},   [Pins]) 	-> baldr_lamp_gpio:start_link(Pins);
start_link( {type, ws281x}, [Led]) 		-> baldr_lamp_ws281x:start_link(Led). 

set(Pid, false) -> 
	set(Pid, {color, 0, 0, 0});
set(Pid, true)  -> 
	set(Pid, {color, 255, 255, 255});
set(Pid, Color) -> 
	Pid ! {lamp_set, self(), Color},
	receive {lamp_set_r, Pid} -> ok end.