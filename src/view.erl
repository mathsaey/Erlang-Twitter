% view.erl
% Mathijs Saey
% Multicore Programming

% This module implements a "view".
% A View is a process that guarantees fast reads.
% To do so, it does not take care of updating itself, 
% instead, other processes do this, after which they push
% the data to the view.

% Read requests get priority over update requests,
% this implies that views under heavy load will return
% older data.

% The data that a view contains is simply a list of elements.
% The view does not care about the exact semantics of these elements.

-module(view).
-export([start/1]).
-export([read/4, update/2]).

% The amount of elements on a
% single page.
-define(PAGE_LENGTH, 10).

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
read(ViewPid, DestPid, Tag, Page) -> ViewPid ! {read, DestPid, Tag, Page}, ok.

% Update the contents of a view.
%
% ViewPid
%		The view to update.
% Content
%		Data to add to the view.
%
update(ViewPid, Content) -> ViewPid ! {update, Content}, ok.

% --------------------- %
% Convenience Functions %
% --------------------- %

% Send data to a destination.
%
% Dest
%	The destination to send the data to.
% Tag
%	The tag to add to the message send.
% Data
%	The complete dataset
% Page 
%	The page to fetch.
%	Fetches __all__ the data if the page is 0.
%	An empty list is returned if the page does not exist.
%
send_data(Dest, Tag, Data, 0) -> Dest ! {Tag, Data};
send_data(Dest, Tag, Data, 1) -> Dest ! {Tag, lists:sublist(Data, ?PAGE_LENGTH)};
send_data(Dest, Tag, Data, Page) -> 
	Start_idx = ((Page - 1) * ?PAGE_LENGTH) + 1,
	Page_content = 
		try   lists:sublist(Data, Start_idx, ?PAGE_LENGTH)
		catch error:function_clause -> [] 
		end,
	Dest ! {Tag, Page_content}.

% Add new data to old data.
%
% Performance note:
%	Erlang copies the left element of ++, not the right one
%	so the new data should be the left element. This has the
%	added benefit that new data is found at the start of the data list.
update_data(Old, New) -> [New] ++ Old.

% ---------------- %
% Request Handling %
% ---------------- %

% Start a view with no known data.
start(Manager) -> start(Manager, []).

% Start a view with some data.
start(Manager, Data) -> spawn(fun() -> view_loop(Manager, Data) end).

% View actor loop.
% Read messages get priority
% over updates.
view_loop(Manager, Data) ->
	receive
		{read, Dest, Tag, Page} -> 
			send_data(Dest, Tag, Data, Page), 
			view_loop(Manager, Data)
	after 0 -> 
		receive
			{read, Dest, Tag, Page} -> 
				send_data(Dest, Tag, Data, Page), 
				view_loop(Manager, Data);
			{update, Content} -> 
				New_Data = update_data(Data, Content),
				view_loop(Manager, New_Data)
		end
	end.

% --------- %
% Test Code %
% --------- %

-include_lib("eunit/include/eunit.hrl").

viewEmpty_test() ->
	V = start(manager),

	read(V, self(), someTag, 0),
	receive
		Data0 -> ?assertMatch({someTag, []}, Data0)
	end,

	read(V, self(), someTag, 1),
	receive
		Data1 -> ?assertMatch({someTag, []}, Data1)
	end.

viewOrder_test() ->
	V = start(manager),

	lists:foreach(
		fun(El) -> update(V, El) end,
		lists:seq(1, 10)),

	% Wait since read requests have priority
	timer:sleep(500),
	read(V, self(), someTag, 0),

	receive
		Data -> ?assertMatch({someTag, [10,9,8,7,6,5,4,3,2,1]}, Data)
	end.

viewPages_test() ->
	V = start(manager),
	L = lists:seq(1, 100),
	R = lists:reverse(L),

	lists:foreach(fun(El) -> update(V, El) end, L),

	timer:sleep(500),

	read(V, self(), all, 0),
	receive
		DataAll -> ?assertMatch({all, R}, DataAll)
	end,

	read(V, self(), empty, 20),
	receive
		DataEmpty -> ?assertMatch({empty, []}, DataEmpty)
	end,

	read(V, self(), first, 1),
	receive
		Data1 -> ?assertMatch({first, [100, 99, 98, 97, 96, 95, 94, 93, 92, 91]}, Data1)
	end,

	read(V, self(), fourth, 4),
	receive
		Data4 -> ?assertMatch({fourth, [70, 69, 68, 67, 66, 65, 64, 63, 62, 61]}, Data4)
	end.