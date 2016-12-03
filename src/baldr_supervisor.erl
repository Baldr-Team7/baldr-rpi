-module(baldr_supervisor).
-behaviour(supervisor).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link_shell/0]).

-export([init/1]).

%% ====================================================================
%% Internal functions
%% ====================================================================
start_link_shell() ->
		{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE,[]),
		unlink(Pid).


start_link() ->
     {ok, Pid} = supervisor:start_link({local, ?MODULE}, 
          ?MODULE, []),
     {ok, Pid}.

init([]) ->
	io:format("~p (~p) starting...~n", [{local,?MODULE}, self()]),
	

RestartStrategy = one_for_all,
MaxRestart = 10,
MaxSecondsBetweenRestarts = 5,
	
Flags = {RestartStrategy, MaxRestart, MaxSecondsBetweenRestarts},


Restart = transient,
	
Shutdown = 2000,

Type = worker,

ChildSpecification = {baldr_gen_server, {baldr_gen_server, start_link, []}, Restart, Shutdown,Type, [baldr_gen_server]},
	
{ok, {Flags, [ChildSpecification]}}.