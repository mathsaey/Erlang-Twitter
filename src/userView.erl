% userView.erl
% Mathijs Saey
% Multicore Programming

% This module creates a custom view.
% This view is reponsible for updating and
% fetching user data

-module(userView).
-export([start/1, addFollower/2, addSubcription/2]).
-export([getId/2, getName/2, getFollowers/2, getSubscriptions/2]).

% --------- %
% Interface %
% --------- %

% Start a new userview
start(User) -> view:start(
	fun(Data, {Destination, Tag}) -> readData(Data, Destination, Tag) end,
	fun(Data, {Tag, New}) -> updateData(Data, New, Tag) end,
	User
	).

% Add a follower to a user view.
addFollower(ViewPid, Follower) -> view:write(ViewPid, {follower, Follower}).

% Add a subscription to a user view.
addSubcription(ViewPid, Subscription) -> view:write(ViewPid, {subscription, Subscription}).

% Get various user elements from a view.
getId(ViewPid, Destination) -> view:read(ViewPid, {Destination, id}).
getName(ViewPid, Destination) -> view:read(ViewPid, {Destination, name}).
getFollowers(ViewPid, Destination) -> view:read(ViewPid, {Destination, followers}).
getSubscriptions(ViewPid, Destination) -> view:read(ViewPid, {Destination, subcriptions}).

% ----------- %
% Convenience %
% ----------- %

updateData(Data, New, follower) -> account:addFollower(Data, New);
updateData(Data, New, subscription) -> account:addSubcription(Data, New).

readData(Data, Dest, id) -> Dest ! {id, account:id(Data)}, ok;
readData(Data, Dest, name) -> Dest ! {name, account:name(Data)}, ok;
readData(Data, Dest, followers) -> Dest ! {followers, account:followers(Data)}, ok;
readData(Data, Dest, subcriptions) -> Dest ! {subcriptions, account:subscriptions(Data)}, ok.

% ---- %
% Test %
% ---- %

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
	V = start(account:create(0, "Mathijs")),

	getId(V, self()),
	receive {id, Id} -> ?assertMatch(0, Id) end,

	getName(V, self()),
	receive {name, Name} -> ?assertMatch("Mathijs", Name) end,

	getFollowers(V, self()),
	receive {followers, F} -> ?assertMatch([], F) end,

	getSubscriptions(V, self()),
	receive {subcriptions, S} -> ?assertMatch([], S) end,

	addFollower(V, 1),
	addFollower(V, 2),
	addSubcription(V, 3),
	addSubcription(V, 4),

	view:update(V, manager),
	timer:sleep(500),

	getFollowers(V, self()),
	receive {followers, FLst} -> ?assertMatch([2,1], FLst) end,
	getSubscriptions(V, self()),
	receive {subcriptions, SLst} -> ?assertMatch([4,3], SLst) end.