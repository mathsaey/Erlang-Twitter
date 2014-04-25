% userView.erl
% Mathijs Saey
% Multicore Programming

% This module creates a custom view.
% This view is reponsible for updating and
% fetching user data

-module(userView).
-export([start/1, addFollower/2, addSubcription/2]).
-export([getId/2, getFollowers/2, getSubscriptions/2]).

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
addSubcription(Id, Subscription) -> write(Id, {subscription, Subscription}).

% Get various user elements from a view.
getId(Id, Destination) -> read(Id, {Destination, id}).
getFollowers(Id, Destination) -> read(Id, {Destination, followers}).
getSubscriptions(Id, Destination) -> read(Id, {Destination, subcriptions}).

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
updateData(Data, New, subscription) -> account:addSubcription(Data, New).

readData(Data, Dest, id) -> Dest ! {id, account:id(Data)}, ok;
readData(Data, Dest, followers) -> Dest ! {followers, account:followers(Data)}, ok;
readData(Data, Dest, subcriptions) -> Dest ! {subcriptions, account:subscriptions(Data)}, ok.

% ---- %
% Test %
% ---- %

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
	A = account:create(0),
	start(A),

	getId(0, self()),
	receive {id, Id} -> ?assertMatch(0, Id) end,

	getFollowers(0, self()),
	receive {followers, F} -> ?assertMatch([], F) end,

	getSubscriptions(0, self()),
	receive {subcriptions, S} -> ?assertMatch([], S) end,

	addFollower(0, 1),
	addFollower(0, 2),
	addSubcription(0, 3),
	addSubcription(0, 4),

	timer:sleep(500),

	getFollowers(0, self()),
	receive {followers, FLst} -> ?assertMatch([2,1], FLst) end,
	getSubscriptions(0, self()),
	receive {subcriptions, SLst} -> ?assertMatch([4,3], SLst) end.