%%% File: cc.erl
%%% Description: This is the moduel that allows to communicate with the credit
%%% card provider.

-module(cc).
-export([is_valid/3, transaction/4]).
-export([valid_date/1, valid_card/1, start_server/0, add_card/3,
	add_funds/2, add_ReferenceId/2]).
-export([stop/0, init/1, terminate/2, handle_call/3, handle_cast/2]).
-behavior(gen_server).

-include("credit_card.hrl").

% Spawns the credit card srever process that calls the init callback function
% - Input:
% - Outout: {ok, pid}
start_server() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).

% Stops the server process
stop() ->
        gen_server:cast(?MODULE, stop).

% The init functions initialises the server
% - Input:
% - Output: loop data that is passed between callback functions.
init(_Args) ->
	cc_db:new(),
        {ok, null}.

% Terminate call
terminate(_Reason, _Loopdata) ->
        user_db:close().

handle_cast(stop, LoopData) ->
        {stop, normal, LoopData}.

% Handles the message that is send to the server for adding a card to the cc
% provider. The card number is again not added in clear text and therefore
% sha hash is added.
handle_call({add_card, CardNumber, ExpDate, BillAddress}, _From ,LoopData) ->
	Hashbin = crypto:hash(sha, integer_to_list(CardNumber)),
	Hash = crypto:bytes_to_integer(Hashbin),
        Reply = cc_db:add(Hash, ExpDate, BillAddress),
        {reply, Reply, LoopData};

% Handles the message that is send to the server for adding funds to a card.
handle_call({add_funds, CardNumber, Funds}, _From, LoopData) ->
	Reply = cc_db:change_funds(add, CardNumber, Funds),
	{reply, Reply, LoopData};

% Handles the message that is send to the server for adding the referenceId
% information to a given credit card record at the provider
handle_call({add_ReferenceId, CardNumber, ReferenceId}, _From, LoopData) ->
	Reply = cc_db:add_ReferenceId(CardNumber, ReferenceId),
	{reply, Reply, LoopData};

% Handles the message that is send to the server for checking validy of a card.
% - This call checks if the given credit card number is an Integer
% - This call checks if the Address passed from the user is equal to the address
%   at the CC provider.
% - This call checks if the Expiration Date is valid
% - This call checks if the Credit Card Number has valid lenght
% - This call checks if the Credit Card Number Hash is known to the CC provider
handle_call({is_valid, BillAddr, CardNumber, ExpDate}, _From, LoopData) ->
	Reply = case is_integer(CardNumber) of
		  true ->
		   Hashbin = crypto:hash(sha, integer_to_list(CardNumber)),
		   Hash = crypto:bytes_to_integer(Hashbin),
		   (BillAddr =:= cc_db:get_bill_address(Hash)) and valid_date(ExpDate)
		   and valid_card(CardNumber) and cc_db:is_defined(Hash);
		  false -> false
		end,
	{reply, Reply, LoopData}.


%% Functional Interface

% Updates the ReferenceId for a given card number
% - Input: CardNumber, ReferenceId
% - Output: ok
add_ReferenceId(CardNumber, ReferenceId) ->
	gen_server:call(?MODULE, {add_ReferenceId, CardNumber, ReferenceId}).

% Adds a new credit card to the cc provider database
% - Input: CardNumber, ExpDate, BillAddress
% - Output: ok
add_card(CardNumber, ExpDate, BillAddress) ->
	gen_server:call(?MODULE, {add_card, CardNumber, ExpDate, BillAddress}).

% Adds funds to an existing credit card
% - Input: CardNumber, Funds
% - Output: {CardNumber, NewFunds}
add_funds(CardNumber, Funds) ->
	gen_server:call(?MODULE, {add_funds, CardNumber, Funds}).

% This functions checks if a given credit card number is valid
% - Input: BillingAddress, CardNumber, ExpirationDate
% - Output: true | false
is_valid(BillAddr, CardNumber, ExpDate) ->
	gen_server:call(?MODULE, {is_valid, BillAddr, CardNumber, ExpDate}).

% Verifies a given Card Number
% - Input: CardNumber
% - Output: true | false
%
% Assumptions:
% - A given CardNumber has between 12 and 16 digits
%
% This simplified check only tests if the CardNumber is between
% 12 and 16 digits.
valid_card(CardNo) ->
	Length  = case is_integer(CardNo) of
			true -> length(integer_to_list(CardNo));
			false -> length(CardNo)
		  end,
	(Length =< 16) and (Length >= 12).

% Verifies if the given ExpirationDate is valid
% - Input: {ExpMonth, ExpYear}
% - Output: true | false
%
% valid_date checks if the given expiration date has not expired.
valid_date({ExpMonth, ExpYear}) ->
	{{Year, Month, _}, _} = calendar:local_time(),
	if
         ExpYear > Year   -> true;
	 ExpYear =:= Year ->
		if
		 Month =< ExpMonth -> true;
		 true 		   -> false
		end;
	 true 		  -> false
	end.


% Starts a transaction. Before, verifies if Card Credentials are valid.
% - Input: BillingAddress, CardNumber, ExpirationDate, Price
% - Output: {ok, TrxId} in case of success.
%	    {error, invalid_card} in case of invalid Credit Card
%	    {error, insufficient_funds} in case of not enough money on account
transaction(BillAddr, CardNumber, ExpDate, Price) ->
	CC_check = cc_db:is_defined(CardNumber) and
		   (BillAddr =:= cc_db:get_bill_address(CardNumber)) and
		   (ExpDate =:= cc_db:get_exp_date(CardNumber)),
	case CC_check and valid_date(ExpDate) of
	 true  ->
		case Price =< cc_db:get_funds(CardNumber) of
		 true -> TrxId = random:uniform(1000000),
			 cc_db:change_funds(remove, CardNumber, Price),
			 {ok, TrxId};
		 false -> {error, insufficient_funds}
		end;
	 false ->
		{error, invalid_card}
	end.
