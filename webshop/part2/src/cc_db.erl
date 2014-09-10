%%% File: cc_db.erl
%%% Description: Holds the credit card server data

-module(cc_db).
-include("credit_card.hrl").
-export([new/0, close/0]).
-export([add/3, lookup/1, change_funds/3, add_ReferenceId/2]).
-export([get_bill_address/1, get_funds/1, is_defined/1, get_exp_date/1]).

%% Basic functionality for opening and closing the database

% Creates a new in memory table (ets table)
% - Input:
% - Output: ok
new() ->
	ets:new(refTableCC, [named_table, public, set]),
	ok.

% Deletes the ets table
close() ->
	ets:delete(refTableCC).

%% Database manipulation functions

% Adds a new credit card to the ets table
% - Input: CardNumber, ExpDate, BillAddress
% - Output: ok
add(CardNumber, ExpDate, BillAddress) ->
	Address = proplists:get_value(address, BillAddress),
	Name = proplists:get_value(name, BillAddress),
	City = proplists:get_value(city, BillAddress),
	Country = proplists:get_value(country, BillAddress),
	BillAddrNew = #billAddress{address = Address, clientName = Name,
				   city = City, country = Country},
	C = #creditCardProv{cardNo = CardNumber, expDate = ExpDate,
			    billAddress = BillAddrNew, owner = Name},
	insert(CardNumber, C),
	ok.

% Inserts a new credit card into the ets table with key CardNumber
% - Input: CardNumber, C
% - Output:
%
% The card number is a sha hash, and not clear text.
insert(CardNumber, C) ->
	ets:insert(refTableCC, {CardNumber, C}).

% Starts a lookup of the given CardNumber in the ets table
% - Input: CardNumber
% - Output: {ok,CreditCard} | {error, instance}
lookup(CardNumber) ->
	case ets:lookup(refTableCC, CardNumber) of
		[{CardNumber, C}] -> {ok, C};
		[] 	     -> {error, instance}
	end.


%% Credit Card manipulation functions

% Adds or removes funds to a given credit card account
% - Input: Operation (Add or Rem) CardNumber, Amount of Funds to be added | removed
% - Output: {CardNumber, NewFunds}
change_funds(Operation, CardNumber, FundsChange) ->
        case lookup(CardNumber) of
          % CardNumber found
          {ok, CreditCard} ->
                Funds    = CreditCard#creditCardProv.funds,
		FundsNew = case Operation of 
				add -> Funds + FundsChange;
				remove -> Funds - FundsChange
			   end,
                Cnew   = CreditCard#creditCardProv{funds = FundsNew},
                insert(CardNumber, Cnew), % insert the new updated credit card.
                {CardNumber, FundsNew};
          % CardNumber not found
          {error,instance} -> {error, "invalid CardNumber"}
        end.

% Adds all relevant person related data in the credit card provider db
% - Input: CardNumber, ReferenceId
% - Output: ok
add_ReferenceId(CardNumber, ReferenceId) ->
        case lookup(CardNumber) of
          % CardNumber found
          {ok, CreditCard} ->
                Cnew   = CreditCard#creditCardProv{refId = ReferenceId},
                insert(CardNumber, Cnew), % insert the new updated credit card.
		ok;
          % CardNumber not found
          {error,instance} -> {error, "invalid CardNumber"}
        end.


% Checks if the given card number is in the ets table
% - Input: CardNumber
% - Output: true | false
is_defined(CardNumber) ->
	case lookup(CardNumber) of
          % CardNumber found
          {ok, _CreditCard} -> true;
          % CardNumber not found
          {error,instance} -> false
        end.


%% Getter Functions

% Returns the billing address of a given card number
% - Input: CardNumber
% - Output: BillAddress
get_bill_address(CardNumber) ->
	case lookup(CardNumber) of
          % CardNumber found
          {ok, CreditCard} ->
                CreditCard#creditCardProv.billAddress;
          % CardNumber not found
          {error,instance} -> {error, "invalid CardNumber"}
        end.

% Returns the funds of a given card number
% - Input: CardNumber
% - Output: Funds
get_funds(CardNumber) ->
	case lookup(CardNumber) of
          % CardNumber found
          {ok, CreditCard} ->
                CreditCard#creditCardProv.funds;
          % CardNumber not found
          {error,instance} -> {error, "invalid CardNumber"}
        end.

% Returns the expiration date of a given card number
% - Input: CardNumber
% - Output: ExpDate
get_exp_date(CardNumber) ->
	case lookup(CardNumber) of
          % CardNumber found
          {ok, CreditCard} ->
                CreditCard#creditCardProv.expDate;
          % CardNumber not found
          {error,instance} -> {error, "invalid CardNumber"}
        end.
