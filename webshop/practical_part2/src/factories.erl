%%% File : factories.erl
%%% Description: Application behavior, responsible for starting the application
-module(factories).
-behaviour(application).

% API
-export([start/2, stop/1]).

% Responsible for starting the Application
start({takeover, OtherNode}, _Args) ->
	io:format("Node ~p will remain active ~n",[OtherNode]),
	{ok, self()};
start(normal, _Args) ->
	sup:start_link().
 
stop(_State) ->	
	ok.
