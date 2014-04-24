% view.erl
% Mathijs Saey
% Multicore Programming

% This module implements a "view".
% A View is a process that contains a collection of data.
% The view can simply read or modify this data.

-module(view).
-export([start/3, read/2, write/2, update/2]).

% --------- %
% Interface %
% --------- %

% Start a view with no known data.
%
% ReadFunc
%		The function that will read the data of this view.
%		It should be able to receive 2 parameters.
%		The first of these 2 parameters is the Data that 
%		the view currently contains.
%		The second is a tuple of arguments that were passed
%		along with the read request.
% WriteFunc
%		The function that will update the data of this view.
%		It takes the same arguments as the ReadFunc, but it should
%		also return the new data of the view.
% Data
%		The data that this view starts with.
%
start(ReadFunc, WriteFunc, Data) -> 
	spawn(fun() -> readLoop(ReadFunc, WriteFunc, Data) end).

% Send a read request to a view.
%
% ViewPid
%		The view to send the request to.
% Args
%		The arguments to add to the read request.
%
read(ViewPid, Args) -> ViewPid ! {read, Args}, ok.

% Update the contents of a view.
%
% ViewPid
%		The view to update.
% Args
%		The arguments to add to the
%		write request.
%
write(ViewPid, Args) -> ViewPid ! {write, Args}, ok.

% Tell a view to start updating.
%
% ViewPid
%		The view that should start updating.
% Manager
%		The manager that requests the update phase to start.
%
update(ViewPid, Manager) -> ViewPid ! {start_update, Manager}, ok.

% ---------------- %
% Request Handling %
% ---------------- %

readLoop(ReadFunc, WriteFunc, Data) ->
	receive
		{read, Args} -> 
			ReadFunc(Data, Args), 
			readLoop(ReadFunc, WriteFunc, Data)
	after 0 -> 
		receive
			{read, Args} -> 
				ReadFunc(Data, Args), 
				readLoop(ReadFunc, WriteFunc, Data);
			{start_update, Manager} -> 
				updateLoop(ReadFunc, WriteFunc, Data, Manager)
		end
	end.

updateLoop(ReadFunc, WriteFunc, Data, Manager) ->
	receive
		{write, Args} ->
			New_Data = WriteFunc(Data, Args),
			updateLoop(ReadFunc, WriteFunc, New_Data, Manager)
	after 0 ->
		% Notify the manager
		readLoop(ReadFunc, WriteFunc, Data)
	end.


% ----- %
% Tests %
% ----- %

-include_lib("eunit/include/eunit.hrl").

startTestView() -> start(
	fun(Data, Dst) -> Dst ! Data, ok end,
	fun(Data, New) -> [New] ++ Data end,
	[]
	).

viewEmpty_test() ->
	V = startTestView(),
	read(V, self()),
	
	receive Data0 -> ?assertMatch([], Data0) end.

viewOrder_test() ->
	V = startTestView(),

	lists:foreach(
		fun(El) -> write(V, El) end,
		lists:seq(1, 10)),

	update(V, manager),

	% Wait since read requests have priority
	timer:sleep(500),
	read(V, self()),

	receive Data -> ?assertMatch([10,9,8,7,6,5,4,3,2,1], Data) end.

viewPriority_test() ->
	V = startTestView(),
	write(V, data),
	read(V, self()),

	receive Lst -> ?assertMatch([], Lst) end.