-module(baldr_client).
-export([start_link/0, start_link_hey/0, start/0, hey/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================


start_link_hey() ->
	baldr_supervisor:start_link_shell(), hey().

start_link() ->
	baldr_supervisor:start_link_shell(), baldr_gen_server:start().

hey() -> baldr_gen_server:hey().

start() ->
	baldr_gen_server:start().



