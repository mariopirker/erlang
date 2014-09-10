%% File: person.hrl
%% Description: Include file for a person that uses the shop
%% Requires: random.erl

-define(MAX_REFID, 1000000000).

% Defines a record for the billing address
% Record fields:
% - address: {int(), atom()}. contains a tuple of street number and street name
% - clientName: atom(). contains the name of the person
% - city: atom(). contains the name of the city where the person lives
% - country: atom(). contains the name of the country where the person lives
-record(billAddress, {address,
		      clientName,
		      city,
		      country}).


% Defines a record for a person that uses the shopping cart system
% Record fields:
% - refId: %int(). generates a random number between 1 and 1000000
% - cart: %[{atom(),int()}]. contains a list of {item, amount}
% - userName: %atom(). contains the username
% - billAddress: #billAddress. contains the record billAddress
% - cardNumber: %int(). contains a sha hash of the credit card number
% - expDate: {int(), int()}. contains tuple of ExpirationMonth and ExpirationDate
% - transactions: %[{cart,int()},int()]. contains a list of finished transactions
%   a list element consists of the purchased cart + price and transactionId
-record(person, {refId = user_db:generate_refId(?MAX_REFID),
		 cart = [{ski, 0}, {bike, 0}, {surfboard, 0}, {skateboard, 0}],
		 userName,
		 billAddress = #billAddress{},
		 cardNumber,
		 expDate,
		 transactions = []}).
