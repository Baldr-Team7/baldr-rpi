-module(ws281x_nif).
-export([start/0, ws281x_nif/2]).

start()-> ok = erlang:load_nif("priv/ws281x_nif", 0).
ws281x_nif(C, L) -> "NIF library not loaded".