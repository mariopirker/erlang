%%% File : factory.erl
%%% Description: API and gen_server code for shopping cart system

-module(factory).
-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([start_server/1]).
-export([skateboard/2, bike/2, surfboard/2, ski/2, view_cart/1,
	billing_address/2, credit_card/3, buy/1]).
-export([prep_message_items/2]).
-behaviour(gen_server).

-include("credit_card.hrl").

%% Functional interface and exported Client Functions
%% API to the outside world

% Spawns the server process that calls the init callback function
% - Input: FileName
% - Output: {ok, pid}
start_server(FileName) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

% Stops the server process
stop() ->
	gen_server:cast(?MODULE, stop).

% Adds a new shopper to the shopping card system
% - Input: UserName
% - Output: {ok, RefId}
%
% The returned RefId is a unique number that is used for the remaining
% of the shopping process for the given UserName
start_link(UserName) ->
	RefId = user_db:add(UserName),
	{ok, RefId}.


%% Callback functions

% The init functions initialises the server
% - Input: FileName
% - Output: loop data that is passed between callback functions.
%
% As my server does not need any LoopData (all data is stored within the ETS
% table), null is returned.
init(FileName) ->
	user_db:new(FileName),
	{ok, null}.

% Terminate call
terminate(_Reason, _Loopdata) ->
	user_db:close().

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

% Handles the message that is send to the server for adding skateboards to the
% cart.
handle_call({skateboard, ReferenceId, N}, _From, LoopData) ->
	Reply = user_db:update_cart(ReferenceId, skateboard, N),
	UserName = user_db:get_user_name(ReferenceId),
	Message = prep_message_items(Reply, skateboards),
	webclient:reply(UserName, Message),
        {reply, Reply, LoopData};

% Handles the message that is send to the server for adding surfboards to the
% cart.
handle_call({surfboard, ReferenceId, N}, _From, LoopData) ->
	Reply = user_db:update_cart(ReferenceId, surfboard, N),
	UserName = user_db:get_user_name(ReferenceId),
	Message = prep_message_items(Reply, surfboards),
	webclient:reply(UserName, Message),
	{reply, Reply, LoopData};

% Handles the message that is send to the server for adding ski to the cart.
handle_call({ski, ReferenceId, N}, _From, LoopData) ->
	Reply = user_db:update_cart(ReferenceId, ski, N),
	UserName = user_db:get_user_name(ReferenceId),
	Message = prep_message_items(Reply, skis),
	webclient:reply(UserName, Message),
        {reply, Reply, LoopData};

% Handles the message that is send to the server for adding bikes to the cart.
handle_call({bike, ReferenceId, N}, _From, LoopData) ->
	Reply = user_db:update_cart(ReferenceId, bike, N),
	UserName = user_db:get_user_name(ReferenceId),
	Message = prep_message_items(Reply, bikes),
	webclient:reply(UserName, Message),
        {reply, Reply, LoopData};

% Handles the message that is send to the server for viewing the shopping cart.
handle_call({view_cart, ReferenceId}, _From, LoopData) ->
	Reply = user_db:get_cart(ReferenceId),
	UserName = user_db:get_user_name(ReferenceId),
	Message = {cart, Reply},
	webclient:reply(UserName, Message),
	{reply, Reply, LoopData};

% Handles the message that is send to the server for updating the billing address.
handle_call({billing_address, ReferenceId, BillAddress}, _From, LoopData) ->
	Reply = user_db:update_bill_address(ReferenceId, BillAddress),
	{reply, Reply, LoopData};

% Handles the message that is send to the server for adding the credit card
% Mind that we first make the check on the real credit card and then store the
% hash inside the ets table (see user_db:update_credit_card).
handle_call({credit_card, ReferenceId, CardNumber, ExpData}, _From,
	    LoopData) ->
	BillAddress = user_db:get_bill_address(ReferenceId),
	Reply = case cc:is_valid(BillAddress, CardNumber, ExpData) of
	         true  ->
		  Hashbin = crypto:hash(sha, integer_to_list(CardNumber)),
		  Hash = crypto:bytes_to_integer(Hashbin),
		  cc:add_ReferenceId(Hash, ReferenceId),
		  user_db:update_credit_card(ReferenceId, Hash, ExpData),
		  ok;
		 false ->
		  {error, card_invalid}
		end,
	{reply, Reply, LoopData};

% Handles the message that is send to the server for buying the elements of the
% cart.
handle_call({buy, ReferenceId}, _From, LoopData) ->
	{_Cart, Price} = user_db:get_cart(ReferenceId),
	BillAddress = user_db:get_bill_address(ReferenceId),
	{CardNumber, ExpDate} = user_db:get_credit_card(ReferenceId),
	Reply = case cc:transaction(BillAddress, CardNumber, ExpDate, Price) of
		 {ok, TrxId} 		     ->
			user_db:add_transaction(ReferenceId, TrxId),
			{ok, user_db:get_cart(ReferenceId)};
		 {error, invalid_card}	     ->
			{error, billing_info};
		 {error, insufficient_funds} ->
			{error, credit_info}
		end,
	UserName = user_db:get_user_name(ReferenceId),
	% Responsible for printing output to the web client
	case Reply of
		{ok, Cart} ->
			webclient:reply(UserName, {buy, Cart}),
			user_db:clear_cart(ReferenceId); % wipe the cart
		{error, _} ->
			webclient:reply(UserName, {buy, Reply})
	end,
	{reply, Reply, LoopData}.

%% Functional API

% Adds N skateboards to the cart of user with RefId
% - Input: ReferenceId, N
% - Output: [{added | removed, N}, {total, Total}]
skateboard(ReferenceId,N) ->
	gen_server:call(?MODULE, {skateboard, ReferenceId, N}).

% Adds N surfboards to the cart of user with RefId
% - Input: ReferenceId, N
% - Output: [{added | removed, N}, {total, Total}]
surfboard(ReferenceId,N) ->
	gen_server:call(?MODULE, {surfboard, ReferenceId, N}).

% Adds N ski to the cart of user with RefId
% - Input: ReferenceId, N
% - Output: [{added | removed, N}, {total, Total}]
ski(ReferenceId,N) ->
	gen_server:call(?MODULE, {ski, ReferenceId, N}).

% Adds N bikes to the cart of user with RefId
% - Input: ReferenceId, N
% - Output: [{added | removed, N}, {total, Total}]
bike(ReferenceId,N) ->
	gen_server:call(?MODULE, {bike, ReferenceId, N}).

% Prints the shopping cart of the person with the given ReferenceId
% - Input: ReferenceId
% - Output: {cart,price}
view_cart(ReferenceId) ->
	gen_server:call(?MODULE, {view_cart, ReferenceId}).

% Updates the billing address of the person with the given ReferenceId
% - Input: ReferenceId, BillingAddress
% - Output: ok | {error, [Items]}
billing_address(ReferenceId, BillAddress) ->
	gen_server:call(?MODULE, {billing_address, ReferenceId, BillAddress}).

% Validates the given Credit Card and adds the CardNumber (as hash) and the
% ExpirationDate to record of the person with the given ReferenceId
% - Input: ReferenceId, CardNumber, ExpData
% - Output: ok | {error, invalid_card}
credit_card(ReferenceId, CardNumber, ExpData) ->
	gen_server:call(?MODULE, {credit_card, ReferenceId, CardNumber, ExpData}).

% Starts the transaction and buys the elements of the person for the given
% ReferenceId
% - Input: ReferenceId
% - Output: {ok, {[{Item, Count}], Value}} | {error, billing_info} |
%           {error, credit_info}
buy(ReferenceId) ->
	gen_server:call(?MODULE, {buy, ReferenceId}).

% Prepares the Message for the webclient
% - Input: Reply, Item
% - Output: {add|remove, Item, Amount, Total}
prep_message_items(Reply, Item) ->
	case Reply of
		[{added, X}, {total, Y}] -> {add, Item, X, Y};
		[{removed, X}, {total, Y}] -> {remove, Item, X, Y}
	end.
