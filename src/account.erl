% account.erl
% Mathijs Saey
% Multicore Programming

% This file declares the interface of 
% the user data type.
% User is reserved by erlang, hence the confusing
% module name.

-module(account).
-export([create/1]).
-export([addSubscription/2, addFollower/2]).
-export([id/1, subscriptions/1, followers/1]).

-record (user,  {id, subscriptions = [], followers = []}).

% Create a user. 
%
% Id
%	The unique identifier for the user.
%	The module creating the user is reponsible
%	for guaranteeing the uniqueness of the id.
%
create(Id) -> #user{id = Id}.

% Getters
% -------

% Get the subscriptions of a user.
subscriptions(User) -> User#user.subscriptions.

% Get the followers of a user.
followers(User) -> User#user.followers.

% Get the id of a user.
id(User) -> User#user.id.

% Modifiers
% ---------

% Add another subscription to a user.
%
% Sub
%	A user id.
%
addSubscription(User, Sub) ->
	Subs = subscriptions(User),
	New  = [Sub] ++ Subs,
	User#user{subscriptions = New}.


% Add another follower to a user.
%
% Follower
%	A user id.
%
addFollower(User, Follower) -> 
	Followers = followers(User),
	New  = [Follower] ++ Followers,
	User#user{followers = New}.