% view.erl
% Mathijs Saey
% Multicore Programming

% This module implements a "view".
% A View is a process that contains a collection of data.
% The view can simply read or modify this data.

-module(view).
-export([start/4, stop/1, read/2, write/2, update/1, duplicate/1]).

% --------- %
% Interface %
% --------- %

% Start a view with no known data.
%
% Manager
%		The viewmanager managing this view.
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
start(Manager, ReadFunc, WriteFunc, Data) -> 
	readLoop(Manager, ReadFunc, WriteFunc, Data).

% Tell a view to stop activity after
% handling it's remaining requests.
%
% ViewPid
%		The view to send the request to.
% 
stop(ViewPid) -> ViewPid ! stop, ok.

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
%
update(ViewPid) -> ViewPid ! start_update, ok.

% ----------- %
% Duplication %
% ----------- %

% Request the full state of a view.
% This includes the manager, read and write functions,
% the data that the view contains as well as the current message queue.
%
getState(ViewPid, Destination) -> ViewPid ! {state, Destination}, ok.

% Add all the updates of a source view's
% message queue to a view.
% 
addUpdates(Data, _, []) -> 
	Data;

addUpdates(Data, WriteFunc, [{write, Args}|Tail]) -> 
	New_Data = WriteFunc(Data, Args), 
	addUpdates(New_Data, WriteFunc, Tail);

addUpdates(Data, WriteFunc, [_Head|Tail]) -> 
	addUpdates(Data, WriteFunc, Tail).

% Duplicate the source view.
duplicate(Source) -> 
	getState(Source, self()),
	receive
		{state, {Manager, ReadFunc, WriteFunc, Data}, Lst} -> 
			New_Data = addUpdates(Data, WriteFunc, Lst),
			viewGroup:startFinished(Manager, self()),
			readLoop(Manager, ReadFunc, WriteFunc, New_Data)
	end.

% ---------------- %
% Request Handling %
% ---------------- %

readLoop(Manager, ReadFunc, WriteFunc, Data) ->
	receive
		{read, Args} -> 
			ReadFunc(Data, Args), 
			readLoop(Manager, ReadFunc, WriteFunc, Data);
		{state, Dest} ->
			{messages, Lst} = erlang:process_info(self(), messages),
			Dest ! {state, {Manager, ReadFunc, WriteFunc, Data}, Lst},
			readLoop(Manager, ReadFunc, WriteFunc, Data)

	after 0 -> 
		receive
			{read, Args} -> 
				ReadFunc(Data, Args),
				viewGroup:readFinished(Manager),
				readLoop(Manager, ReadFunc, WriteFunc, Data);
			{state, Dest} ->
				{messages, Lst} = erlang:process_info(self(), messages),
				Dest ! {state, {Manager, ReadFunc, WriteFunc, Data}, Lst},
				readLoop(Manager, ReadFunc, WriteFunc, Data);
			start_update -> 
				updateLoop(Manager, ReadFunc, WriteFunc, Data);
			stop -> finished
		end
	end.

updateLoop(Manager, ReadFunc, WriteFunc, Data) ->
	receive
		{write, Args} ->
			New_Data = WriteFunc(Data, Args),
			updateLoop(Manager, ReadFunc, WriteFunc, New_Data)
	after 0 ->
		viewGroup:updateFinished(Manager),
		readLoop(Manager, ReadFunc, WriteFunc, Data)
	end.

% ----- %
% Tests %
% ----- %

-include_lib("eunit/include/eunit.hrl").

startTestView() -> spawn(fun() -> 
	start(
		manager,
		fun(Data, Dst) -> Dst ! Data, ok end,
		fun(Data, New) -> [New] ++ Data end,
		[])
	end).

viewEmpty_test() ->
	V = startTestView(),
	read(V, self()),
	
	receive Data0 -> ?assertMatch([], Data0) end.

viewOrder_test() ->
	V = startTestView(),

	lists:foreach(
		fun(El) -> write(V, El) end,
		lists:seq(1, 10)),

	update(V),

	% Wait since read requests have priority
	timer:sleep(500),
	read(V, self()),

	receive Data -> ?assertMatch([10,9,8,7,6,5,4,3,2,1], Data) end.

viewPriority_test() ->
	V = startTestView(),
	write(V, data),
	read(V, self()),

	receive Lst -> ?assertMatch([], Lst) end.

viewDuplicate_test() ->
	V = startTestView(),

	write(V, 1),
	write(V, 2),

	timer:sleep(500),
	write(V, 3),

	D = spawn(fun() -> duplicate(V) end),

	read(D, self()),
	receive Data -> ?assertMatch([3,2,1], Data) end.