% viewGroup.erl
% Mathijs Saey
% Multicore Programming

% This module implements view groups.

-module (viewGroup).
-export([create/4, read/2, write/2]).
-export([readFinished/1, updateFinished/2, startFinished/2]).

% The amount of requests/view before
% views will be deleted.
-define(REQUEST_LOWER_LIMIT, 1).

% The amount of requests/view before
% new views will be created.
-define(REQUEST_UPPER_LIMIT, 3).

% ----------------- %
% External Requests %
% ----------------- %

create(Name, ReadFunc, WriteFunc, Data) -> 
	Atom = list_to_atom(Name),

	Monitor = spawn(fun() -> monitorLoop(Atom, false, queue:new(), 0, 0) end),
	spawn(fun() -> view:start(Monitor, ReadFunc, WriteFunc, Data) end),
	register(Atom, Monitor),
	pg2:create(Atom).

read(Name, Args) -> 
	Atom = list_to_atom(Name),
	View = pg2:get_closest_pid(Atom),
	view:read(View, Args),

	Atom ! read_start.

write(Name, Args) ->
	Atom = list_to_atom(Name), 
	lists:foreach(
		fun(View) -> view:write(View, Args) end,
		pg2:get_local_members(Atom)),

	Atom ! write.

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
% Writes
%		The amount of writes the view had to
%		process.
%
updateFinished(Monitor, Writes) -> Monitor ! {update_finished, Writes}, ok.

% Notify a manager that a new view has been created.
%
% Monitor
%		The manager to notify.
% View
%		The view that was created.
%
startFinished(Monitor, View) -> Monitor ! {start_finished, View}, ok.

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
% Updating
%		The view that is currently being updated
% Waiting
%		The views that are waiting to be updated
% Reads
%		The amount of active read requests
% Views
%		The amount of view serving requests 
%		(includes the view being updated)
%

% Create extra processes if needed.
monitorLoop(Group, Updating, Waiting, Reads, Views) 
	when (Reads / Views) > ?REQUEST_UPPER_LIMIT ->
		Source = queue:peek_r(Waiting),
		spawn(view, duplicate, [Source]),
		monitorLoop(Group, Updating, Waiting, Reads, Views + 1);

% Remove processes if needed.
monitorLoop(Group, Updating, Waiting, Reads, Views) 
	when Views > 1 and ((Reads / Views) =< ?REQUEST_UPPER_LIMIT) ->	
		View = pg2:get_closest_pid(Group),
		pg2:leave(Group, View),
		view:stop(View),
		monitorLoop(Group, Updating, Waiting, Reads, Views - 1);

% Handle requests.
monitorLoop(Group, Updating, Waiting, Reads, Views) ->
	receive
		{start_finished, View} -> 
			N_Waiting = queue:in(Waiting, View),
			pg2:join(Group, View),
			monitorLoop(Group, Updating, N_Waiting, Reads, Views)
	after 0 ->
		receive
			% Add freshly created duplicates
			{start_finished, View} -> 
				N_Waiting = queue:in(Waiting, View),
				pg2:join(Group, View),
				monitorLoop(Group, Updating, N_Waiting, Reads, Views);

			% Stop updating if no more writes are present.
			{update_finished, 0} -> 
				N_Waiting = queue:in(Waiting, Updating),
				monitorLoop(Group, false, N_Waiting, Reads, Views);
			% Cycle the currently updating view.
			{update_finished, _} -> 
				Tmp_Queue = queue:in(Waiting, Updating),
				{{value, N_Updating}, N_Waiting} = queue:out(Tmp_Queue),
				pg2:join(Group, Updating),
				pg2:leave(Group, N_Updating),
				view:update(N_Updating),
				monitorLoop(Group, N_Updating, N_Waiting, Reads, Views);

			% Restart updating upon write
			write when not Updating ->
				{{value, N_Updating}, N_Waiting} = queue:out(Waiting),
				pg2:leave(Group, N_Updating),
				view:update(N_Updating),
				monitorLoop(Group, N_Updating, N_Waiting, Reads, Views);
			write ->
				monitorLoop(Group, Updating, Waiting, Reads, Views);

			% Update amount of reads
			read_start -> 
				monitorLoop(Group, Updating, Waiting, Reads + 1, Views);
			read_finished -> 
				monitorLoop(Group, Updating, Waiting, Reads - 1, Views)
		end
	end.