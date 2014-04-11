% types.erl
% Mathijs Saey
% Multicore Programming

% This file declares the interface of the data types 
% of the project. Users and tweets.

-module(types).
-export([tweet_create/2, tweet_user/1, tweet_content/1, tweet_timestamp/1]).
-export([user_create/2, user_subs/1, user_name/1, user_id/1, user_add_sub/2]).

-record (tweet, {timestamp, user_id, content}).
-record (user,  {id, name, subscriptions = []}).

% ------ %
% Tweets %
% ------ %

% Create a tweet.
% The timestamp field is added by 
% fetching the current time.
%
% User
%	The id of the user, an integer.
% Content
%	The contents of the tweet, a string.
tweet_create(User, Content) ->
	Time = erlang:now(),
	#tweet{timestamp = Time, user_id = User, content = Content}.

% Getters
% -------

% Get the id of the user that tweeted this tweet.
tweet_user(Tweet) -> Tweet#tweet.user_id.

% Get the contents of the tweet.
tweet_content(Tweet) -> Tweet#tweet.content.

% Get the timestamp of the tweet.
tweet_timestamp(Tweet) -> Tweet#tweet.timestamp.

% ----- %
% Users %
% ----- %

% Create a user. 
%
% Id
%	The unique identifier for the user.
%	The module creating the user should be reponsible
%	for guaranteeing the uniqueness of the id.
% Name
%	The name of the user, a string.
user_create(Id, Name) -> #user{id = Id, name = Name}.

% Getters
% -------

% Get the subscriptions of a user.
user_subs(User) -> User#user.subscriptions.

% Get the name of a user.
user_name(User) -> User#user.name.

% Get the id of a user.
user_id(User) -> User#user.id.

% Modifiers
% ---------

% Add another subscription to a user.
%
% Sub
%	A user id.
user_add_sub(User, Sub) ->
	Subs = user_subs(User),
	New  = Subs ++ [Sub],
	User#user{subscriptions = New}.
