%% File: credit_cards.hrl
%% Description: Include file for a credit card for the credit card provider.
-include("person.hrl").

% Defines a record for credit card information of the cc provider
% Record fields:
% - cardNo: int(). contains a sha hash of the credit card number
% - expDate: {int(), int()}. contains tuple of ExpirationMonth and ExpirationDate
% - owner: atom(). owner of the credit card
% - billAddress: #billAddress. contains the record billAddress
% - refId: int(). contains the refId that is linked to this card
% - funds: int(). amount of money that is on the credit card account
-record(creditCardProv, {cardNo,
		         expDate,
		         owner,
		         billAddress = #billAddress{},
		         refId,
		         funds = 0}).
