%%% File: test.erl
%%% Description: Test for Practical Part I

-module(test).
-export([start_test/2]).
-export([run/2]).

start_test(UserName, Funds) ->
	io:format("###################################################~n", []),
        io:format("        Compiling Everything in the directory ~n", []),
        io:format("###################################################~n", []),
        cover:compile_directory(),
        io:format("~n", []),
	run(UserName, Funds).

run(User, Funds) ->
	io:format("###################################################~n", []),
        io:format("###################################################~n", []),
        io:format("       Starting Test for Practical Part I~n", []),
        io:format("###################################################~n", []),
        io:format("###################################################~n", []),
	% Hard Coded Card Details and Address
	CardNumber = 8347237233121,
	Hashbin = crypto:hash(sha, integer_to_list(CardNumber)),
        CardNumberHash = crypto:bytes_to_integer(Hashbin),
	ExpDate = {7, 2014},
	BillAddr = [{address, {12, "Guarininistrasse"}},
		    {name, "Mario Pirker"},
		    {city, "Volders"},
		    {country, "Austria"}],
	DbName = "Test",
	io:format("~n",[]),

	% Starting Procedure of the factory and Credit Card Provider
        io:format("###################################################~n", []),
	io:format("       Starting the shopping cart server ~n",[]),
        io:format("###################################################~n", []),
	factory:start_server(DbName),
	{ok, RefId} = factory:start_link(User),
	io:format("Initiate Users shopping cart. Users RefId ~p ~n~n",[RefId]),

        io:format("###################################################~n", []),
	io:format("         Starting the credit card server ~n",[]),
        io:format("###################################################~n", []),
	cc:start_server(),
	cc:add_card(CardNumber, ExpDate, BillAddr),
	cc:add_funds(CardNumberHash, Funds),
	io:format("Added ~p funds to the credit card account ~n~n",[Funds]),

	% The user adds some elements to the cart
        io:format("###################################################~n", []),
	io:format("       Starting the shopping experience ~n",[]),
        io:format("###################################################~n", []),
	factory:skateboard(RefId, 3),
	io:format("~n",[]),
	factory:surfboard(RefId, 0),
	io:format("~n",[]),
	factory:skateboard(RefId, -2),
	io:format("~n",[]),
	factory:skateboard(RefId, -3),
	io:format("~n",[]),
	factory:ski(RefId, 2),
	io:format("~n",[]),
	factory:skateboard(RefId, 4),
	io:format("~n",[]),

	% The user wants to view the cart
	factory:view_cart(RefId),

	% Now adding billing Address to record of the person
	factory:billing_address(RefId, BillAddr),

	% Simulating incorrect CardId
	%CreditCardOutF = factory:credit_card(RefId, {4540, 1234, 5678}, {7, 2014}),
	%io:format("~p ~n", [CreditCardOutF]),

	% Finally adding the credit card information to the record of the person
	factory:credit_card(RefId, CardNumber, ExpDate),

	% The user buys the items in the cart
	Buyed = factory:buy(RefId),
	io:format("~p ~n",[Buyed]),
	io:format("###################################################~n", []),
        io:format("###################################################~n", []),
        io:format("           End of Test Practical PartI  ~n", []),
        io:format("###################################################~n", []),
        io:format("###################################################~n", []).
