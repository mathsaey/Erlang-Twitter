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

% The data that a view contains is simply a list of elements.
% The view does not care about the exact semantics of these elements.

% TODO: Make read use pages for shorter replies (perhaps use a seperat process for this).
% TODO: Add dynamic scaling.

-module(view).
-export([start/0]).
-export([read/4, update/2]).

% The amount of elements on a
% single page.
-define(PAGE_LENGTH, 3).

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
%		A list of data to add to the view.
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
send_data(Dest, Tag, Data, Page) -> 
	Page_content = 
		try   lists:sublist(Data, Page * ?PAGE_LENGTH, ?PAGE_LENGTH)
		catch error:function_clause -> [] 
		end,
	Dest ! {Tag, Page_content}.

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
		{read, Dest, Tag, Page} -> 
			send_data(Dest, Tag, Data, Page), 
			view_loop(Data);
		{update, Content} -> 
			New_Data = update_data(Data, Content),
			view_loop(New_Data)
	end.
