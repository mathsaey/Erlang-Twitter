% tweet.erl
% Mathijs Saey
% Multicore Programming

% This file declares the interface of 
% the tweet data type.

-module(tweet).
-export([create/2, user/1, content/1, timestamp/1, insert/2, getPage/2]).
-record (tweet, {timestamp, user_id, content}).

% The amount of tweets on a
% single page.
-define(PAGE_LENGTH, 10).

% Create a tweet.
% The timestamp field is added by 
% fetching the current time.
%
% User
%	The id of the user, an integer.
% Content
%	The contents of the tweet, a string.
create(User, Content) ->
	#tweet{timestamp = erlang:now(), user_id = User, content = Content}.

% Getters
% -------

% Get the id of the user that tweeted this tweet.
user(Tweet) -> Tweet#tweet.user_id.

% Get the contents of the tweet.
content(Tweet) -> Tweet#tweet.content.

% Get the timestamp of the tweet.
timestamp(Tweet) -> Tweet#tweet.timestamp.

% Tweet Lists
% -----------

% Insert a tweet into a sorted list of tweets.
% The tweet will be inserted before the first 
% encountered tweet that was sent after this tweet.
insert(Tweet, [Head|_] = Lst) 
	when Tweet#tweet.timestamp >= Head#tweet.timestamp -> [Tweet| Lst];

insert(Tweet, [Head|Tail]) -> [Head | insert(Tweet, Tail)];
insert(Tweet, []) -> [Tweet].

% Get a "page" from a list
% of tweets.
%
% Lst
%	The complete list of tweets
% Page 
%	The page to fetch.
%	Fetches __all__ the data if the page is 0.
%	An empty list is returned if the page does not exist.
%
getPage(Lst, 0) -> Lst;
getPage(Lst, 1) -> lists:sublist(Lst, ?PAGE_LENGTH);
getPage(Lst, P) -> 
	Start_idx = ((P - 1) * ?PAGE_LENGTH) + 1,
	try   lists:sublist(Lst, Start_idx, ?PAGE_LENGTH)
	catch error:function_clause -> [] 
	end.

% ---- %
% Test %
% ---- %

-include_lib("eunit/include/eunit.hrl").

order_test() ->
	T1 = tweet:create(0, "First!"),
	T2 = tweet:create(0, "Second"),
	T3 = tweet:create(0, "Last:("),

	L = lists:foldl(
		fun(El, Acc) -> insert(El, Acc) end,
		[],
		[T1, T2, T3] 
	),

	?assertMatch([T3, T2, T1], L).

page_test() ->
	L = lists:seq(1, 100),

	?assertMatch(L, getPage(L, 0)),
	?assertMatch([], getPage(L, 20)),
	?assertMatch([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], getPage(L, 1)),	
	?assertMatch([31, 32, 33, 34, 35, 36, 37, 38, 39, 40], getPage(L, 4)).
