% tweetView.erl
% Mathijs Saey
% Multicore Programming

% This module creates a custom view.
% This view is reponsible for updating and
% fetching lists of tweets.

-module(tweetView).
-export([start/1, read/4, write/2]).

% The amount of elements on a
% single page.
-define(PAGE_LENGTH, 10).

% --------- %
% Interface %
% --------- %

% Start the tweet view.
% Simply starts a view with the correct 
% read and write functions.
start(Manager) -> view:start(
	Manager,
	fun(Data, {Dest, Tag, Page}) -> send_data(Dest, Tag, Data, Page) end,
	fun(Data, {Tweet}) -> tweet:insert(Tweet, Data) end,
	[]
	).

% Send a read request to a tweet view.
%
% ViewPid
% 	The view to send the request to.
% DestPid
% 	The Pid of the process waiting for a reply.
% Tag
% 	A tag (an atom) that will be added to the reply.
% 	sent to DestPid
% Page
% 	The page to fetch.
% 	Fetches __all__ the data if the page is 0.
% 	An empty list is returned if the page does not exist.
%
read(ViewPid, DestPid, Tag, Page) -> view:read(ViewPid, {DestPid, Tag, Page}).


% Update the contents of a view.
%
% ViewPid
% 	The view to update.
% Tweet
% 	The tweet to add to the view.
%
write(ViewPid, Tweet) -> view:write(ViewPid, {Tweet}).

% ----------- %
% Convenience %
% ----------- %

% Get the data of a view.
% And send it to the process waiting for it.
%
% Dest
%	The destination to send the data to.
% Tag
%	The tag to add to the message send.
% Data
%	The complete dataset
% Page 
%	The page to fetch.
%	Fetches __all__ the data if the page is 0.
%	An empty list is returned if the page does not exist.
%
send_data(Dest, Tag, Data, 0) -> Dest ! {Tag, Data};
send_data(Dest, Tag, Data, 1) -> Dest ! {Tag, lists:sublist(Data, ?PAGE_LENGTH)};
send_data(Dest, Tag, Data, Page) -> 
	Start_idx = ((Page - 1) * ?PAGE_LENGTH) + 1,
	Page_content = 
		try   lists:sublist(Data, Start_idx, ?PAGE_LENGTH)
		catch error:function_clause -> [] 
		end,
	Dest ! {Tag, Page_content}.


% ---- %
% Test %
% ---- %

-include_lib("eunit/include/eunit.hrl").

order_test() ->
	V = start(manager),
	T1 = tweet:create(0, "First!"),
	T2 = tweet:create(0, "Second"),
	T3 = tweet:create(0, "Last:("),

	write(V, T2),
	write(V, T1),
	write(V, T3),

	timer:sleep(500),
	read(V, self(), tweets, 0),

	receive
		{tweets, Lst} -> ?assertMatch([T3, T2, T1], Lst)
	end.

pages_test() ->
	V = view:start(
		manager,
		fun(Data, {Dest, Tag, Page}) -> send_data(Dest, Tag, Data, Page) end,
		fun(Data, New) -> [New] ++ Data end,
		[]),

	L = lists:seq(1, 100),
	R = lists:reverse(L),

	lists:foreach(fun(El) -> view:write(V, El) end, L),

	timer:sleep(500),

	read(V, self(), all, 0),
	receive
		{all, DataAll} -> ?assertMatch(R, DataAll)
	end,

	read(V, self(), empty, 20),
	receive
		{empty, DataEmpty} -> ?assertMatch([], DataEmpty)
	end,

	read(V, self(), first, 1),
	receive
		{first, Data1} -> ?assertMatch([100, 99, 98, 97, 96, 95, 94, 93, 92, 91], Data1)
	end,

	read(V, self(), fourth, 4),
	receive
		{fourth, Data4} -> ?assertMatch([70, 69, 68, 67, 66, 65, 64, 63, 62, 61], Data4)
	end.