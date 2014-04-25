% userView.erl
% Mathijs Saey
% Multicore Programming

% This module creates a custom view.
% This view is reponsible for updating and
% fetching user data

-module(userView).
-export([start/1, addFollower/2, addSubscription/2]).
-export([getFollowers/1, getSubscriptions/1]).

% --------- %
% Interface %
% --------- %

% Start a new userview
start(User) -> viewGroup:create(
	"usr" ++ integer_to_list(account:id(User)),
	fun(Data, {Destination, Tag}) -> readData(Data, Destination, Tag) end,
	fun(Data, {Tag, New}) -> updateData(Data, New, Tag) end,
	User
	).

% Add a follower to a user view.
addFollower(Id, Follower) -> write(Id, {follower, Follower}).

% Add a subscription to a user view.
addSubscription(Id, Subscription) -> write(Id, {subscription, Subscription}).

% Get various user data from a view,
% and wait for the reply.
%
getFollowers(Id) -> 
	read(Id, {self(), followers}),
	receive {followers, Lst} -> Lst end.

getSubscriptions(Id) ->
	read(Id, {self(), subscriptions}),
	receive {subscriptions, Lst} -> Lst end.

% ----------- %
% Convenience %
% ----------- %

read(Id, Args) -> 
	Name = "usr" ++ integer_to_list(Id),
	viewGroup:read(Name, Args).
write(Id, Args) -> 
	Name = "usr" ++ integer_to_list(Id),
	viewGroup:write(Name, Args).

updateData(Data, New, follower) -> account:addFollower(Data, New);
updateData(Data, New, subscription) -> account:addSubscription(Data, New).

readData(Data, Dest, id) -> Dest ! {id, account:id(Data)}, ok;
readData(Data, Dest, followers) -> Dest ! {followers, account:followers(Data)}, ok;
readData(Data, Dest, subscriptions) -> Dest ! {subscriptions, account:subscriptions(Data)}, ok.

% ---- %
% Test %
% ---- %

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
	A = account:create(0),
	start(A),

	?assertMatch([], getFollowers(0)),
	?assertMatch([], getSubscriptions(0)),

	addFollower(0, 1),
	addFollower(0, 2),
	addSubscription(0, 3),
	addSubscription(0, 4),

	timer:sleep(500),

	?assertMatch([2,1], getFollowers(0)),
	?assertMatch([4,3], getSubscriptions(0)).