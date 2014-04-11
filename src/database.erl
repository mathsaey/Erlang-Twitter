% database.erl
% Mathijs Saey
% Multicore Programming

% This file declares the "database" of the project.
% It is the sole point of the system that has all the 
% actual data.

% However, caching and load balancing algorithms may mean it
% is not always up to date.

-module(database).

% ------------ %
% Writing data %
% ------------ %

% Add a new "account".
add_user(Name) -> ok.

% Add a subscription to somebody else.
add_subscription(Subscriber, Target) -> ok.

% Add a tweet to the system.
store_tweet() -> ok.

% ------------ %
% Reading data %
% ------------ %

% Get all the tweets of a given user.
get_tweets(User) -> ok.

% Get all the users somebody is subscribed to.
get_subscriptions(User) -> ok.
