% account.erl
% Mathijs Saey
% Multicore Programming

% This file declares the interface of 
% the user data type.
% User is reserved by erlang, hence the confusing
% module name.

-module(account).
-export([create/2, subs/1, name/1, id/1, add_sub/2]).

-record (user,  {id, name, subscriptions = []}).

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
subs(User) -> User#user.subscriptions.

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
add_sub(User, Sub) ->
	Subs = subs(User),
	New  = [Sub] ++ Subs,
	User#user{subscriptions = New}.