% ring.erl
% Mathijs Saey
% Multicore Programming

% This module implements a ring ADT.

-module(ring).
-export([create/0, empty/1, singleEl/1, insert/2, current/1, setCurrent/2, previous/1, turn/1, map/2, filter/2]).

% -------- %
% Ring ADT %
% -------- %

% Create a ring
create() -> {[],[]}.

% See if the ring is empty
empty({[],[]}) -> true;
empty(_) -> false.

% Check if the ring contains more than a single element.
singleEl({[], [_]}) -> true;
singleEl(_) -> false.

% Add an item to the ring, the item is added as the
% previous item of the ring.
insert(Item, {[], []}) -> {[], [Item]};
insert(Item, {L,R}) -> {[Item|L], R}.

% Return the current item 
current({[],[]}) -> empty;
current({_, [H|_]}) -> H.

% Change the current item of the ring
setCurrent(_, {[], []}) -> empty;
setCurrent(I, {L, [_|T]}) -> {L, [I|T]}.

% Return the previous item
previous({[], []}) -> empty;
previous({[], [I]}) -> I;
previous({[H|_], _}) -> H.

% "Rotate" the ring.
turn({[],[]}) -> {[],[]};
turn({[],[I]}) -> {[],[I]};
turn({L, [R]}) -> {[R], lists:reverse(L)};
turn({L, [H|T]}) -> {[H|L], T}.

% Perform a map operation over all the elements in the ring
map(F, {L, R}) -> {lists:map(F, L), lists:map(F, R)}.

% Filter out elements in the ring
filter(F, {L,R}) -> 
	case {lists:filter(F, L), lists:filter(F, R)} of
		{[], []} -> {[],[]};
		{[H],[]} -> {[], [H]};
		{[H|T], []} -> {[H], lists:reverse(T)};
		{[], [H|T]} -> {lists:reverse(T), [H]};
		{LF, RF} -> {LF, RF}
	end.

% ----- %
% Tests %
% ----- %

-include_lib("eunit/include/eunit.hrl").

generate_ring() -> lists:foldl(
	fun(El, Acc) -> insert(El, Acc) end,
	create(),
	lists:seq(1,10)
).

empty_test() ->
	R = create(),
	?assert(empty(R)).

insert_test() -> 
	R = generate_ring(),

	lists:foldl(
		fun(El, Acc) -> 
			Curr = current(Acc),
			?assertMatch(Curr, El), 
			turn(Acc)
		end,
		R, lists:seq(1,10)
		).

previous_test() ->
	R = generate_ring(),

	lists:foldl(
		fun(_, {Ring, Prev}) -> 
			Val = previous(Ring),
			?assertMatch(Val , Prev), 
			{turn(Ring), current(Ring)}
		end,
		{R, 10}, lists:seq(1,10)
	).

map_test() ->
	R = generate_ring(),
	M = map(fun(El) -> El + 1 end, R),

	lists:foldl(
		fun(El, Acc) -> 
			Curr = current(Acc),
			?assertMatch(Curr, El + 1), 
			turn(Acc)
		end,
		M, lists:seq(1,10)
	).

filter_test() ->
	R = generate_ring(),
	F = filter(fun(El) -> El rem 2 /= 0 end, R),

	lists:foldl(
		fun(El, Acc) -> 
			Curr = current(Acc),
			?assertMatch(Curr, El), 
			turn(Acc)
		end,
		F, lists:seq(1,10,2)
	).