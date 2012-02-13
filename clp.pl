:- module(clp, [domain/3, eq/2, lt/2, gt/2, label/1, all_different/1]).

:- use_module(library(atts)).
:- attribute dom/2.

verify_attributes(_Var, _Value, _Goals).

domain([], _, _).
domain([Var|Rest], Lower, Upper) :- 
          put_atts(Var, dom(Lower, Upper)), 
          domain(Rest, Lower, Upper).

merge_upper_bound(Left, Right) :- get_atts(Left, dom(LeftLower, LeftUpper)),
                               get_atts(Right, dom(_, RightUpper)),
                               NewUpper is RightUpper - 1,
                               NewUpper < LeftUpper, !,
                               put_atts(Left, dom(LeftLower, NewUpper)).
merge_upper_bound(_, _).

merge_lower_bound(Left, Right) :- get_atts(Left, dom(LeftLower, _)),
                               get_atts(Right, dom(RightLower, RightUpper)),
                               NewLower is LeftLower + 1,
                               RightLower < NewLower, !,
                               put_atts(Right, dom(NewLower, RightUpper)).
merge_lower_bound(_, _).

set_upper_bound(Var, Value) :-  get_atts(Var, dom(Lower, _)),
                                dom(Var, Lower, Value).
set_lower_bound(Var, Value) :-  get_atts(Var, dom(_, Upper)),
                                dom(Var, Value , Upper).


lt(Left, Right) :-  % 5 < A
                    nonvar(Left), var(Right),
                    Lower is Left + 1,
                    set_lower_bound(Right, Lower),
                    when(Right, Left < Right).
lt(Left, Right) :-  % A < 5
                    var(Left), nonvar(Right),
                    Upper is Right - 1,
                    set_upper_bound(Left, Upper),
                    freeze(Left, Left < Right).
lt(Left, Right) :- % A < B
                    var(Left), var(Right),
                    merge_lower_bound(Left, Right),
                    merge_upper_bound(Left, Right),
                    freeze(Left, freeze(Right, Left < Right)),
                    freeze(Right, freeze(Left, Left < Right)).
lt(Left, Right) :- nonvar(Left), nonvar(Right), raise.


gt(Left, Right) :- lt(Right, Left).

eq(Left, Right) :- var(Left), var(Right), freeze(Left, freeze(Right, Left == Right)).
eq(Left, Right) :- var(Left), atomic(Right), domain([Left], Right, Right), freeze(Left, Left == Right).
eq(Left, Right) :-  var(Right), atomic(Left), 
                    domain([Right], Left, Left), 
                    freeze(Right, Left == Right).

all_different(List) :- all_different(List, List).
all_different([], _).
all_different([H|T], List) :- all_different2(H, List), all_different(T, List).


all_different2(_, []).
all_different2(Var, [H|T]) :- Var == H, !, all_different2(Var, T).
all_different2(Var, [H|T]) :- Var \== H, freeze(Var, freeze(H, Var \== H)),
                              all_different2(Var, T).

% TODO: support for search strategies
label([]).
label([Var|Rest]) :- label2(Var), label(Rest).
label2(Var) :- get_atts(Var, dom(Lower, Upper)), enumerate(Var, Lower, Upper).

enumerate(Var, Lower, Lower) :- Var = Lower.
enumerate(Var, Lower, Upper) :- Lower < Upper, Var = Lower.
enumerate(Var, Lower, Upper) :- Lower < Upper, 
                                A is Lower + 1,
                                A =< Upper, 
                                enumerate(Var, A, Upper).


% :- domain([A, B], 0, 10), A lt B, B eq 7, label(A).
% domain([A, B, C, D, E], 3, 7), all_different([A, B, C, D, E]), label([A, B, C, D, E]). 
