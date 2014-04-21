% account.erl
% Mathijs Saey
% Multicore Programming

% This file declares the interface of 
% the user data type.
% User is reserved by erlang, hence the confusing
% module name.

-module(account).
-export([create/2]).
-export([addSubcription/2, addFollower/2]).
-export([name/1, id/1, subscriptions/1, followers/1]).

-record (user,  {id, name, subscriptions = [], followers = []}).

% Create a user. 
%
% Id
%	The unique identifier for the user.
%	The module creating the user should be reponsible
%	for guaranteeing the uniqueness of the id.
% Name
%	The name of the user, a string.
create(Id, Name) -> #user{id = Id, name = Name}.

% Getters
% -------

% Get the subscriptions of a user.
subscriptions(User) -> User#user.subscriptions.

% Get the followers of a user.
followers(User) -> User#user.followers.

% Get the name of a user.
name(User) -> User#user.name.

% Get the id of a user.
id(User) -> User#user.id.

% Modifiers
% ---------

% Add another subscription to a user.
%
% Sub
%	A user id.
%
addSubcription(User, Sub) ->
	Subs = subscriptions(User),
	New  = [Sub] ++ Subs,
	User#user{subscriptions = New}.


% Add another follower to a user.
%
% Follower
%	A user id.
%
addFollower(User, Follower) -> 
	Subs = followers(User),
	New  = [Follower] ++ Subs,
	User#user{followers = New}.