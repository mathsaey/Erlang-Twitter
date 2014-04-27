% requestHandler.erl
% Mathijs Saey
% Multicore Programming

% This module implements a request handler.

-module(requestHandler).
-export([initialize/0, registerUser/0, subscribe/3]).

% --------- %
% Interface %
% --------- %

initialize() ->
	startGenerator(), 
	ok.

% Start the required views for a user
% and create an entry process for that user.
registerUser() ->
	Id = getId(),
	U = account:create(Id),

	% Start required views
	userView:start(U),
	tweetView:start(tweets, Id),
	tweetView:start(timeline, Id),

	% Start entry process
	{Id, spawn(fun() -> handler() end)}.

% Add a subscription
subscribe(ServerPid, UserId, SubId) ->
    ServerPid ! {self(), subscribe, UserId, SubId},
    receive {subscribed, UserId, SubId} -> ok end.

% ----------------- %
% Tweet dispatching %
% ----------------- %

sendTweet(Sender, UserId, Text) ->
	Tweet = tweet:create(UserId, Text),
	Sender ! {tweet_accepted, tweet:timestamp(Tweet)},
	spawn(fun() -> dispatchTweet(Tweet, UserId) end).

dispatchTweet(Tweet, UserId) ->
	Followers = userView:getFollowers(UserId),
	tweetView:write(tweets, UserId, Tweet),
	tweetView:write(timeline, UserId, Tweet),
	lists:foreach(fun (Id) -> tweetView:write(timeline, Id, Tweet) end, Followers).

% ------------- %
% Subscriptions %
% ------------- %

addSubscription(Sender, UserId, SubId) ->
	Sender ! {subscribed, UserId, SubId},
	spawn(fun () -> dispatchSubscription(UserId, SubId) end).

dispatchSubscription(UserId, SubId) ->
	% Add subscriber + follower
	userView:addSubscription(UserId, SubId),
	userView:addFollower(SubId, UserId),

	% Add all previous tweets by this user.
	tweetView:read(tweets, SubId, self(), 0),
	Tweets = receive {tweets, Lst} -> Lst end,
	lists:foreach(fun(T) -> tweetView:write(timeline, UserId, T) end, Tweets).

% ------- %
% Handler %
% ------- %

handler() ->
	receive
		{Sender, get_timeline, UserId, Page} -> 
			tweetView:read(timeline, UserId, Sender, Page),
			handler();
		{Sender, get_tweets,   UserId, Page} ->
			tweetView:read(tweets, UserId, Sender, Page),
			handler();
		{Sender, tweet,        UserId, Tweet} ->
			sendTweet(Sender, UserId, Tweet), 
			handler();
		{Sender, subscribe,    UserId, SubId} -> 
			addSubscription(Sender, UserId, SubId),
			handler()
	end.

% ----------------- %
% User Id generator %
% ----------------- %

startGenerator() -> register(generator, spawn(fun() -> idGenerator(0) end)).

idGenerator(Current) ->
	receive
		{Sender, generate} -> Sender ! {id, Current}, idGenerator(Current + 1)
	end.

getId() -> 
	generator ! {self(), generate},
	receive {id, Res} -> Res end.