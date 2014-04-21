%% This module provides the protocol that is used to interact with an
%% implementation of a microblogging service.
%%
%% The interface is design to be synchrounous and waits for the reply of the
%% system.
%%
%% This module defines the public API that is supposed to be used for
%% experiments. The implementation specific API in server_single_actor can be
%% adapted as needed. The semantics of the API here should remain unchanged.
-module(server).

%%
%% Exported Functions
%%
-export([get_timeline/3,
		 get_tweets/3,
		 tweet/3]).

%%
%% API Functions
%%

% Request a page of the timeline of a particular user.
% Request results can be 'paginated' to reduce the amount of data to be sent in
% a single response. This is up to the server.
-spec get_timeline(pid(), integer(), integer()) -> [{tweet, integer(), erlang:timestamp(), string()}].
get_timeline(ServerPid, UserId, Page) ->
	ServerPid ! {self(), get_timeline, UserId, Page},
	receive
		{ServerPid, timeline, UserId, Page, Timeline} ->
			Timeline
	end.

% Request a page of tweets of a particular user.
% Request results can be 'paginated' to reduce the amount of data to be sent in
% a single response. This is up to the server.
-spec get_tweets(pid(), integer(), integer()) -> [{tweet, integer(), erlang:timestamp(), string()}].
get_tweets(ServerPid, UserId, Page) ->
	ServerPid ! {self(), get_tweets, UserId, Page},
	receive
		{ServerPid, tweets, UserId, Page, Tweets} ->
			Tweets
	end.

% Submit a tweet for a user.
% (Authorization/security are not regarded in any way.)
-spec tweet(pid(), integer(), string()) -> erlang:timestamp(). 
tweet(ServerPid, UserId, Tweet) ->
	ServerPid ! {self(), tweet, UserId, Tweet},
	receive
		{ServerPid, tweet_accepted, UserId, Timestamp} ->
			Timestamp
	end.
