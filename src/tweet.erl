% tweet.erl
% Mathijs Saey
% Multicore Programming

% This file declares the interface of 
% the tweet data type.

-module(tweet).
-export([create/2, user/1, content/1, timestamp/1, insert/2]).
-record (tweet, {timestamp, user_id, content}).

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


% Convenience
% -----------

% Insert a tweet into a sorted list of tweets.
% The tweet will be inserted before the first 
% encountered tweet that was sent after this tweet.
insert(Tweet, [Head|_] = Lst) 
	when Tweet#tweet.timestamp >= Head#tweet.timestamp -> [Tweet| Lst];

insert(Tweet, [Head|Tail]) -> [Head | insert(Tweet, Tail)];
insert(Tweet, []) -> [Tweet].