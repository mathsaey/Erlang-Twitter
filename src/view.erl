% view.erl
% Mathijs Saey
% Multicore Programming

% This module implements a "view".
% A View is a process that guarantees fast reads.
% To do so, it does not take care of updating itself, 
% instead, other processes do this, after which they push
% the data to the view.

% When a view receives too many requests, it dynamically creates
% new copies of itself.

% The data that a vies contains is simply a list of elements.
% The view does not care about the exact semantics of these elements.

% TODO: Make read use pages for shorter replies (perhaps use a seperat process for this).
% TODO: Add dynamic scaling.

-module(view).
-export([start/0]).
-export([read/3, update/2]).

% -------- %
% Requests %
% -------- %

% Send a read request to a view.
%
% ViewPid
%	The view to send the request to.
% DestPid
%	The Pid of the process waiting for a reply.
% Tag
%	A tag (an atom) that will be added to the reply.
%	sent to DestPid
%
read(ViewPid, DestPid, Tag) -> ViewPid ! {read, DestPid, Tag}, ok.

% Update the contents of a view.
%
% ViewPid
%		The view to update.
% Content
%		A list of data to add to the view.
%
update(ViewPid, Content) -> ViewPid ! {update, Content}, ok.

% --------------------- %
% Convenience Functions %
% --------------------- %

% Send data to a destination.
send_data(Dest, Tag, Data) -> Dest ! {Tag, Data}.

% Add new data to old data.
%
% Performance note:
%	Erlang copies the left element of ++ not the right one
%	so the new data should be the left element. This has the
%	added benefit that new data is found at the start of the data list.
update_data(Old, New) -> [New] ++ Old.

% --------------- %
% Dynamic Scaling %
% --------------- %

% Duplicate a view.
% Effectively creates a new view
% with identical data.
%
% Data
%	The data of a view.
%
duplicate(Data) -> start(Data).

% ---------------- %
% Request Handling %
% ---------------- %

% Start a view with no known data.
start() -> start([]).

% Start a view with some data.
start(Data) -> spawn(fun() -> view_loop(Data) end).

% View actor loop.
view_loop(Data) ->
	receive
		{read, Dest, Tag} -> send_data(Dest, Tag, Data), view_loop(Data);
		{update, Content} -> view_loop(update_data(Data, Content))
	end.
