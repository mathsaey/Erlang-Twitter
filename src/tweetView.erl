% tweetView.erl
% Mathijs Saey
% Multicore Programming

% This module creates a custom view.
% This view is reponsible for updating and
% fetching lists of tweets.

-module(tweetView).
-export([start/2, read/4, write/3]).

% --------- %
% Interface %
% --------- %

% Start the tweet view.
% Simply starts a view with the correct 
% read and write functions.
%
% Type
%		The type of tweet view, timeline or tweets
% UserId
%		The id of the user that this view belongs to.
%
start(Type, UserId) -> viewGroup:create(
	atom_to_list(Type) ++ integer_to_list(UserId),
	fun(Data, {Dest, Tag, Page}) -> sendData(Dest, Tag, Data, Page) end,
	fun(Data, {Tweet}) -> tweet:insert(Tweet, Data) end,
	[]
	).

% Send a read request to a tweet view.
%
% Type
%		The type of tweet view, timeline or usertweets
% UserId
%		The id of the user that this view belongs to.
% DestPid
% 	The Pid of the process waiting for a reply.
% Page
% 	The page to fetch.
% 	Fetches __all__ the data if the page is 0.
% 	An empty list is returned if the page does not exist.
%
read(Type, Id, DestPid, Page) -> 
	Name = atom_to_list(Type) ++ integer_to_list(Id),
	viewGroup:read(Name, {DestPid, Type, Page}).

% Update the contents of a view.
%
% Type
%		The type of tweet view, timeline or usertweets
% UserId
%		The id of the user that this view belongs to.
% Tweet
% 	The tweet to add to the view.
%
write(Type, Id, Tweet) -> 
	Name = atom_to_list(Type) ++ integer_to_list(Id),
	viewGroup:write(Name, {Tweet}).

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
sendData(Dest, Tag, Data, Page) ->
	Dest ! {Tag, tweet:getPage(Data, Page)}.

% ---- %
% Test %
% ---- %

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
	start(tweets, 0),
	read(tweets, 0, self(), 0),
	T = tweet:create(0, "No fun allowed"),

	receive
		Empty -> ?assertMatch({tweets, []}, Empty)
	end,

	write(tweets, 0, T),
	timer:sleep(500),

	read(tweets, 0, self(), 0),
	receive
		Data -> ?assertMatch({tweets, [T]}, Data)
	end.