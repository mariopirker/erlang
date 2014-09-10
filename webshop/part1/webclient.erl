%%% File: webclient.erl
%%% Description: Outputs information to the user of the website.

-module(webclient).
-export([reply/2]).
-export([print_bought_items/1]).

% This function outputs information to the console
% - Input: UserName, Message
% - Output: ok
%
% The message is printed out as site effect to the console.
reply(UserName, Message) ->
	case Message of
	  {add, Item, N, Total} ->
	    io:format("Added ~p ~p to the cart of ~p. ", [N, Item, UserName]),
	    io:format("Total Number of ~p: ~p ~n~n",[Item, Total]);
	  {remove, Item, N, Total} ->
	    io:format("Removed ~p ~p from the cart of ~p. ", [N, Item, UserName]),
	    io:format("Total number of ~p: ~p ~n~n", [Item, Total]);
	  {cart, {Cart, Price}} ->
	    {_, Ski} = proplists:lookup(ski, Cart),
	    {_, Bikes} = proplists:lookup(bike, Cart),
	    {_, SurfBoards} = proplists:lookup(surfboard, Cart),
	    {_, SkateBoards} = proplists:lookup(skateboard, Cart),
	    io:format("The shopping cart for ~p is ~n~n",[UserName]),
	    io:format("ski: ~p ~n", [Ski]),
	    io:format("bike: ~p ~n", [Bikes]),
	    io:format("surfboard: ~p ~n", [SurfBoards]),
	    io:format("skateboard: ~p ~n", [SkateBoards]),
	    io:format("~n",[]),
	    io:format("Total Price: ~p ~n~n", [Price]);
	  {buy, {error, billing_info}} ->
	    io:format("Invalid billing info ~n", []);
	  {buy, {error, credit_info}} ->
	    io:format("Insufficient funds ~n", []);
	  {buy, {Cart, Price}} ->
	    io:format("~p just bought ",[UserName]),
	    print_bought_items(Cart),
	    io:format(". ",[]),
	    io:format("The total price is ~p ~n~n", [Price])
	end,
	ok.

% This function prints out the bought items of a cart to the user
% - Input: Cart
% - Output: As site effect, bought items are printed to the console
print_bought_items(Cart) ->
	    {_, Ski} = proplists:lookup(ski, Cart),
            {_, Bikes} = proplists:lookup(bike, Cart),
            {_, Surfboards} = proplists:lookup(surfboard, Cart),
            {_, Skateboards} = proplists:lookup(skateboard, Cart),
	    if
	     (Ski > 0) and (not (Ski =:= none)) ->
	       io:format("~p skis", [Ski]);
	     true ->
	       io:format("",[])
	    end,
	    if
	     (Bikes > 0) and (not (Bikes =:= none)) ->
	       io:format(", ~p bikes", [Bikes]);
	     true ->
	       io:format("",[])
	    end,
	    if
	     (Surfboards > 0) and (not (Surfboards =:= none)) ->
	       io:format(", ~p surfboards", [Surfboards]);
	     true ->
	       io:format("",[])
	    end,
	    if
	     (Skateboards > 0) and (not(Skateboards =:= none)) ->
	       io:format(", ~p skateboards",[Skateboards]);
	     true ->
	       io:format("",[])
	    end,
	ok.
