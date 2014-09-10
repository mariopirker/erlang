%%% File : sup.erl
%%% Description: Supervisor behavior, supervising the supervisor of the 
%%% shopping cart server.
-module(sup).
-behavior(supervisor).

% API
-export([start_link/0]).

% Supervisors callback
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
	% Create ETS and DETS table
        user_db:open(),

	RestartStrategy = one_for_one, 
	MaxRestarts = 5,
	MaxSecondsBetweenRestarts = 3600, 
	
	RunPolicy = permanent, 
	TimeItHasToShutdown = infinity,
	Type = supervisor,

	CartSup = {serverSup, {cart_sup, start_link,[]}, RunPolicy, 
		TimeItHasToShutdown, Type, [cart_sup]},

	ChildList = [CartSup],

	{ok, {{RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	ChildList}}.	
