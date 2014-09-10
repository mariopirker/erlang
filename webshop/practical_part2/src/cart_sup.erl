%%% File : cart_sup.erl
%%% Description: Supervisor behavior, supervising the shopping cart server
-module(cart_sup).
-behavior(supervisor).

% API
-export([start_link/0, start_factory/0]).

% Supervisors callback
-export([init/1]).

-define(MAX_FACTORIES, 10000000).

%% ===================================================================
%% API functions
%% ===================================================================

% This function starts supervisor
start_link() -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

% This function initializes the shopping cart supervisor
init(_) ->
	RestartStrategy = simple_one_for_one, 
	MaxRestarts = 10,
	MaxSecondsBetweenRestarts = 1800,
	
	RunPolicy = permanent, 
	TimeItHasToShutdown = 2000,
	Type = worker,
	ModulesUsed = [factory],

	CartSrv = {factory, {factory, start_server,[]}, RunPolicy, 
		  TimeItHasToShutdown, Type, ModulesUsed},
	ChildList = [CartSrv],

	{ok, {{RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	ChildList}}.

% This function starts a new child of the shopping cart supervisor
% - Input: Name
% - Output: 
% 
% The children function that is started is a factory server
start_factory() -> 
	Name = random:uniform(?MAX_FACTORIES),	
	supervisor:start_child(cart_sup, [Name]).	
