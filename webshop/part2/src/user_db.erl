%%% File: user_db.erl
%%% Description: Holds the user data

-module(user_db).
-include("person.hrl").
-export([open/0, close/0]).
-export([add/1, lookup/1, update_cart/3, get_cart/1, sum_cart/1, update_bill_address/2,
        get_bill_address/1, get_credit_card/1, update_credit_card/3, clear_cart/1,
        add_transaction/2, get_user_name/1, restore_backup/0]).
-export([sum/2, write/3, delete/2, check_address/1, generate_refId/1]).

-define(DETS_DB,"shopping_cart.db").

%% Basic functionality for opening and closing the database

% Opens the in memory table and opens the database on the disk
% - Input: 
% - Output: ok
open() ->
        % DETS table
        dets:open_file(refTableDisk,[{file, ?DETS_DB}]),
        % ETS table. If already exists, catch error and don't 
	% try to create it again.
	_Ets = case (catch ets:new(refTableRam, [named_table, set])) of
                {'EXIT', {badarg, _}} -> {error, already_exists};
		_ -> ok
	      end,
        % Restore the data
        restore_backup(),
        ok.

% Deletes the ets table of the memory and closes the database file
close() ->
        ets:delete(refTableRam),
        dets:close(refTableDisk).

% Restores the ets table from the dets table
restore_backup() ->
        ets:from_dets(refTableRam, refTableDisk),
        ok.

%% Database manipulation functions

% Adds a new person to the ets table and database
% - Input: Name
% - Output: ReferenceId
add(Name) ->
	P = #person{userName = Name},
	ReferenceId = P#person.refId,
	insert(ReferenceId,P),
	ReferenceId.

% Inserts a person into the ets table with key ReferenceId
% - Input: ReferenceId, P
% - Output:
insert(ReferenceId, P) ->
	ets:insert(refTableRam, {ReferenceId, P}),
	dets:insert(refTableDisk, {ReferenceId, P}).

% Starts a lookup of the given ReferenceId in the ets table
% - Input: ReferenceId
% - Output: {ok,Person} | {error, instance}
lookup(ReferenceId) ->
	case ets:lookup(refTableRam, ReferenceId) of
		[{ReferenceId, P}] -> {ok, P};
		[] 	     -> {error, instance}
	end.


%% Shopping card related functions

% Clears the shopping cart for a given person
% - Input: ReferenceId
% - Output: ok
%
% This function is called after a successful transaction
clear_cart(ReferenceId) ->
	case lookup(ReferenceId) of
          {ok, Person} ->
		Pnew   = Person#person{cart = []},
		insert(ReferenceId, Pnew), % insert the new updated person.
		ok;
          {error, instance} ->
                {error, "invalid ReferenceId"}
	end.

% Returns the Shopping Cart for a given ReferenceId
% - Input: ReferenceId
% - Output: {cart, prize} | {error, "invalid ReferenceId"}
get_cart(ReferenceId) ->
	case lookup(ReferenceId) of
          {ok, Person} ->
                Cart = Person#person.cart,
                {Cart, sum_cart(Cart)};
          {error, instance} ->
                {error, "invalid ReferenceId"}
        end.

% Generates the sum of the Shopping cart
% - Input: Cart (as list)
% - Output: prize (of the complete shopping cart)
sum_cart(Cart) ->
	lists:sum(lists:foldr(fun sum/2,[],Cart)).

% Sum function
% - Input: {Item, N}, Accumulator
% - Output: [Item*N|Accumulator]
%
% Price for the items are hardcoded:
% - ski        = 150
% - skateboard = 50
% - bike      = 175
% - surfboard  = 175
sum({Item, N}, Acc) ->
	case Item of
		ski -> [150*N|Acc];
		skateboard -> [50*N|Acc];
		bike -> [175*N|Acc];
		surfboard -> [175*N|Acc]
	end.

% Updates the Shopping Cart for a given ReferenceId
% - Input: ReferenceId, Item, N
% - Output: [{added | removed, N}, {total, Total}]
%
% When passing a positive Natural Number N the item is added.
% Wwhen passing a negatie Natural Number N the item is removed.
update_cart(ReferenceId, Item, N) ->
	case lookup(ReferenceId) of
	  % ReferenceId found
	  {ok, Person} ->
		Cart    = Person#person.cart,
		Value = proplists:get_value(Item, Cart),
		CartNew = if
                           Value + N < 0  -> write(Item, 0, Cart);
                           Value + N >= 0 -> write(Item, Value+N, Cart)
			  end,
		Pnew   = Person#person{cart = CartNew},
		insert(ReferenceId, Pnew), % insert the new updated person.
		ValNew = proplists:get_value(Item,(Pnew#person.cart)),
		ValUpdated = case proplists:is_defined(Item, Cart) of
				true  ->
				  Val = proplists:get_value(Item , Cart),
                                  if
				   Val =:= 0    ->
					proplists:get_value(Item, CartNew);
                                   Val+N >= 0   -> N;
                                   Val < abs(N) -> Val
                                  end;
				false ->
				  if
                                   ValNew == 0 -> 0;
                                   true        -> N
                                  end
			     end,
		Updated = if
                           N < 0  -> removed;
                           N >= 0 -> added
                          end,
		[{Updated, abs(ValUpdated)}, {total, ValNew}];
	  % ReferenceId not found
	  {error,instance} -> {error, "invalid ReferenceId"}
	end.

% Updates the billing address for a given ReferenceId
% - Input: ReferenceId, BillingAddress
% - Output: ok | {error, [Item]}
update_bill_address(ReferenceId, BillAddress) ->
	case lookup(ReferenceId) of
	 {ok, Person} ->
		case check_address(BillAddress) of
                 ok ->
                   Address = proplists:get_value(address, BillAddress),
                   Name = proplists:get_value(name, BillAddress),
                   City = proplists:get_value(city, BillAddress),
                   Country = proplists:get_value(country, BillAddress),
		   Bold = Person#person.billAddress, % old BillAddress
		   Bnew = Bold#billAddress{address = Address, % new billAddress
				       clientName = Name,
				       city = City,
				       country = Country},
                   Pnew = Person#person{billAddress = Bnew},
                   insert(ReferenceId, Pnew), % insert the new updated person.
                   ok;
                 {error, Items} ->
                   {error, Items}
                end;
	 {error, instance} ->
		{error, "invalid ReferenceId"}
	end.

% Checks if the given Billing Address is valid
% - Input: BillingAddress
% - Output: ok | {error,[Item]
%
% Returns an error if address, name, city or country is not defined
% in the given address.
check_address(BillAddress) ->
	ValAddr = proplists:is_defined(address, BillAddress),
	ValUserName = proplists:is_defined(name, BillAddress),
	ValCity = proplists:is_defined(city, BillAddress),
	ValCountry = proplists:is_defined(country, BillAddress),
	case ValAddr and ValUserName and ValCity and ValCountry of
		true  -> ok;
		false -> Items = [X || X <- [address,name,city,country],
				  not(proplists:is_defined(X, BillAddress))],
			 {error, Items}
	end.

% Returns the Billing Address stored for a given ReferenceId
% - Input: ReferenceId
% - Output: billingAddress | {error, "invalid ReferenceId"}
get_bill_address(ReferenceId) ->
	case lookup(ReferenceId) of
         {ok, Person} ->
		Person#person.billAddress;
         {error, instance} ->
		{error, "invalid ReferenceId"}
        end.

% Returns the user name stored for a given ReferenceId
% - Input: ReferenceId
% - Output: userName
get_user_name(ReferenceId) ->
	case lookup(ReferenceId) of
         {ok, Person} ->
                Person#person.userName;
         {error, instance} ->
                {error, "invalid ReferenceId"}
        end.

% Updates the credit card information for a given user
% - Input: ReferenceId, CardNumber, ExpirationDate
% - Output: ok | {error, "invalid ReferenceId"}
%
% The CardNumber is stored as sha hash for security reasons
update_credit_card(ReferenceId, CardNumber, ExpDate) ->
	case lookup(ReferenceId) of
	  {ok, Person} ->
		Pnew = Person#person{cardNumber = CardNumber, expDate = ExpDate},
		insert(ReferenceId, Pnew), % insert the new updated person.
		ok;
	  {error, instance} ->
		{error, "invalid ReferenceId"}
	end.

% Returns the Credit Card information for a given ReferenceId
% - Input: ReferenceId
% - Output: {cardNumber, ExpirationDate} | {error, "invalid ReferenceId"}
get_credit_card(ReferenceId) ->
	case lookup(ReferenceId) of
	  {ok, Person} ->
		CardNumber = Person#person.cardNumber,
		ExpDate = Person#person.expDate,
		{CardNumber, ExpDate};
	  {error, instance} ->
		{error, "invalid ReferenceId"}
	end.

% Adds a successful transaction to the record of the person
% - Input: ReferenceId, TransactionId
% - Output: ok | {error, "invalid ReferenceId"}
add_transaction(ReferenceId, TrxId) ->
	case lookup(ReferenceId) of
	  {ok, Person} ->
		Cart = get_cart(ReferenceId),
		Trans = Person#person.transactions,
		Pnew = Person#person{transactions = write(TrxId, Cart, Trans)},
		insert(ReferenceId, Pnew), % insert the new updated person.
                ok;
	  {error, instance} ->
		{error, "invalid ReferenceId"}
	end.

% This function generates a new ReferenceId 
% - Input: MaxRefId
% - Output: 
generate_refId(MaxRefId) -> 
	{_, Secs, MicroSecs} = erlang:now(),
	((random:uniform(MaxRefId) - MicroSecs)) + Secs.

% Writes a value into a list and removes old key-value pair if it already
% exists
% - Input: Key, Value, List
% - Output: List containing new {Key, Value} pair
write(Key, Val, List) -> [{Key, Val}|delete(Key, List)].

% Deletes a key value pair from a given list
% - Input: Key, List
% - Output: List without the {Key, _} pair
delete(Key, [{Key,_Val}|List]) -> List;
delete(Key, [Elem|List]) -> [Elem|delete(Key, List)];
delete(_Key, []) -> [].

