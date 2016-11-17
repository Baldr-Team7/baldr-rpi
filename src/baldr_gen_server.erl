-module(baldr_gen_server).

-behaviour(gen_server).
%% ====================================================================
%% API functions
%% ====================================================================
-export([hey/0,start_link/0, start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================


%% Client call
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

hey() ->
	gen_server:call({global,?MODULE}, hey).

start() -> gen_server:call({global, ?MODULE}, start).






%% call back

init([]) ->
	process_flag(trap_exit, true),
	io:format("~p (~p) starting... ~n", [{global, ?MODULE}, self()]),
	{ok, []}.

handle_call(start, _From, State) ->
	Results = baldr_light:start(),
	{reply, Results, State};

handle_call(hey, _From, State) ->
	Results = baldr_light:hey(),
	{reply, Results, State};

handle_call(_Request, _From, State) ->
	{reply, any_thing, State}.

handle_cast(_Request, State) ->
{no_reply, State}.

handle_info(Info, State) ->
	{no_reply, Info, State}.

terminate(_reason, State) ->
io:format("terminating ~p~n", [{global, ?MODULE}]),
ok.

code_change(Old, State, _Extra)->
	{ok, State}.

