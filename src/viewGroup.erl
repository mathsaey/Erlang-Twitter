% viewGroup.erl
% Mathijs Saey
% Multicore Programming

% This module implements view groups.

-module (viewGroup).
-export([create/4, read/2, write/2]).
-export([readFinished/1, updateFinished/2, startFinished/2]).

% The amount of requests/view before
% views will be deleted.
-define(REQUEST_LOWER_LIMIT, 0.5).

% The amount of requests/view before
% new views will be created.
-define(REQUEST_UPPER_LIMIT, 10).

% ----------------- %
% External Requests %
% ----------------- %

create(Name, ReadFunc, WriteFunc, Data) -> 
	Atom = list_to_atom(Name),
	pg2:create(Name),

	Monitor = spawn_link(fun() -> monitorLoop(Name, ring:create(), 0, 1, 0) end),
	spawn_link(fun() -> view:start(Monitor, ReadFunc, WriteFunc, Data) end),
	register(Atom, Monitor),
	waitForGroup(Name).

read(Name, Args) -> 
	Atom = list_to_atom(Name),
	View = pg2:get_closest_pid(Name),

	view:read(View, Args),
	Atom ! read_start.

write(Name, Args) ->
	Atom = list_to_atom(Name), 
	Atom ! {write, Args}.

% ------------- %
% View Requests %
% ------------- %

% Notify a manager that a read request
% has been handled.
%
% Monitor
%		The manager to notify.
%
readFinished(Monitor) -> Monitor ! read_finished, ok.

% Notify a manager that a view is finished
% updating.
%
% Monitor
%		The manager to notify.
%
updateFinished(Monitor, Tag) -> Monitor ! {update_finished, Tag}, ok.

% Notify a manager that a new view has been created.
%
% Monitor
%		The manager to notify.
% View
%		The view that was created.
%
startFinished(Monitor, View) -> Monitor ! {start_finished, View}, ok.

% --------------------- %
% Convenience Functions %
% --------------------- %

% Wait for a group to gain member
% views before returning.
waitForGroup(Name) ->
	case pg2:get_members(Name) of
		[] -> timer:sleep(100);
		_ -> ok
	end.

% View rings are an abstraction on top of rings
% They simply contain the available view and a flag
% that tells us if they should be updated or not.

insertView(View, Ring) -> ring:insert({View, clean}, Ring).
deleteView(View, Ring) -> ring:filter(fun({P, _}) -> P /= View end, Ring).

% Send a write request to all the views, and mark
% them as changed.
setFlags(Ring, Args) -> ring:map(
	fun
		({P, updating}) -> view:write(P, Args);
		({P, _}) -> view:write(P, Args), {P, dirty} 
	end, 
	Ring).

% See if we encounter any elements in the ring
% that need updating. If this is the case, update them.
updateRing(Ring, Group) -> 
	case ring:singleEl(Ring) of 
		true  -> updateSingle(Ring);
		false -> updateNext(Ring, Group, ring:previous(Ring))
	end.

% Find the next element in the ring that needs updating
% If we find it, update it.
% Returns a new ring.
updateNext(Ring, Group, End) ->
	case ring:current(Ring) of
		End -> Ring;
		{_, updating} -> Ring;
		{_, clean} -> 
			updateNext(ring:turn(Ring), Group, End);
		{V, dirty} -> 
			pg2:leave(Group, V),
			view:update(V, add), 
			ring:setCurrent({V, updating}, Ring)
	end.

% See if the only element in the ring needs udpating.
updateSingle(Ring) ->
	case ring:current(Ring) of
		{_, updating} -> Ring;
		{_, clean} -> Ring;
		{V, dirty} -> 
			view:update(V, no_add), 
			ring:setCurrent({V, updating}, Ring)
	end.


% ------------- %
% Group Monitor %
% ------------- %

% Main loop of the group monitor.
% Also checks the amount of read requests per
% view, if this reaches a certain treshold, it
% creates extra views.
%
% Group
%		The group monitored by the monitor
% Ring
%		The ring that holds the active views.
% Reads
%		The amount of active read requests
% Views
%		The amount of view serving requests 
%		This includes the view being updated
%		and views currently being created.
%
% Create extra processes if needed.
monitorLoop(Group, Ring, Reads, Views, Active) 
	when (Reads / Views) > ?REQUEST_UPPER_LIMIT ->
		{Source, _} = ring:previous(Ring),
		spawn(view, duplicate, [Source]),
		monitorLoop(Group, Ring, Reads, Views + 1, Active);

% Remove processes if needed.
monitorLoop(Group, Ring, Reads, Views, Active) 
	when (Active > 1) and ((Reads / Active) < ?REQUEST_LOWER_LIMIT) ->
		View = pg2:get_closest_pid(Group),
		N_Ring = deleteView(View, Ring),
		pg2:leave(Group, View),
		view:stop(View),
		monitorLoop(Group, N_Ring, Reads, Views - 1, Active - 1);

% Handle requests.
monitorLoop(Group, Ring, Reads, Views, Active) ->
	receive
		{start_finished, View} -> 
			pg2:join(Group, View),
			N_Ring = insertView(View, Ring),
			monitorLoop(Group, N_Ring, Reads, Views, Active + 1)
	after 0 ->
		receive
			% Add freshly created duplicates
			{start_finished, View} -> 
				pg2:join(Group, View),
				N_Ring = insertView(View, Ring),
				monitorLoop(Group, N_Ring, Reads, Views, Active + 1);

			% Start updating the views
			{write, Args} ->
				Tmp_Ring = setFlags(Ring, Args),
				New_Ring = updateRing(Tmp_Ring, Group),
				monitorLoop(Group, New_Ring, Reads, Views, Active);

			% Update the next view view
			{update_finished, no_add} ->
				{View,_} = ring:current(Ring),
				Tmp_Ring = ring:setCurrent({View, clean}, Ring),
				New_Ring = updateRing(Tmp_Ring, Group),
				monitorLoop(Group, New_Ring, Reads, Views, Active);

			{update_finished, add} ->
				{View,_} = ring:current(Ring), 
				pg2:join(Group, View),

				Tmp_Ring = ring:setCurrent({View, clean}, Ring),
				New_Ring = updateRing(Tmp_Ring, Group),
				monitorLoop(Group, New_Ring, Reads, Views, Active);

			% Update amount of reads
			read_start -> 
				monitorLoop(Group, Ring, Reads + 1, Views, Active);
			read_finished ->
				monitorLoop(Group, Ring, Reads - 1, Views, Active)
		end
	end.

% ----- %
% Tests %
% ----- %

-include_lib("eunit/include/eunit.hrl").

startTestGroup(Name) -> create(
	Name,	
	fun(Data, Dst) -> Dst ! Data, ok end,
	fun(Data, New) -> [New] ++ Data end,
	[]).

basic_test() ->
	startTestGroup("test0"),
	startTestGroup("test1"),

	read("test0", self()),
	receive Data0 -> ?assertMatch([], Data0) end,
	read("test1", self()),
	receive Data1 -> ?assertMatch([], Data1) end,

	write("test0", 1),

	timer:sleep(500),
	read("test0", self()),
	receive Data2 -> ?assertMatch([1], Data2) end,
	read("test1", self()),
	receive Data3 -> ?assertMatch([], Data3) end.

scale_test() ->
	startTestGroup("scale"),
	Receiver = spawn(fun() -> ok end),

	lists:foreach(
		fun(_) -> read("scale", Receiver) end,
		lists:seq(1, 100)),

	?assert(length(pg2:get_local_members("scale")) > 1).
