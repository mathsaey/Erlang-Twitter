% ring.erl
% Mathijs Saey
% Multicore Programming

% This module implements a ring ADT.

-module(ring).
-export([create/0, empty/1, singleEl/1, insert/2, current/1, previous/1, turn/1, map/2, filter/2]).

% -------- %
% Ring ADT %
% -------- %

create() -> {[],[]}.

empty({[],[]}) -> true;
empty(_) -> false.

singleEl({[], [_]}) -> true;
singleEl(_) -> false.

insert(Item, {[], []}) -> {[], [Item]};
insert(Item, {L,R}) -> {[Item|L], R}.

current({[],[]}) -> empty;
current({_, [H|_]}) -> H.

previous({[], []}) -> empty;
previous({[], [I]}) -> I;
previous({[H|_], _}) -> H.

turn({[],[]}) -> {[],[]};
turn({[],[I]}) -> {[],[I]};
turn({L, [R]}) -> {[R], lists:reverse(L)};
turn({L, [H|T]}) -> {[H|L], T}.

map(F, {L, R}) -> {lists:map(F, L), lists:map(F, R)}.

filter(F, {L,R}) -> 
	case {lists:filter(F, L), lists:filter(F, R)} of
		{[], []} -> {[],[]};
		{LF, []} -> {[], lists:reverse(LF)};
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