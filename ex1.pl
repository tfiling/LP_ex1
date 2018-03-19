countcandg('C').
countcandg('G').

count([], 0).
count([H|T], N) :-
	count(T, Y),
    countcandg(H),
    N is Y+1,
    N > 0.
count([_|T], Y) :-
    count(T, Y).

percentage(N, [H|T]) :- 
	percentage(N, Z, [H|T]).

percentage(N, Z, [H|T]) :- 
	Z is N//2,
	count([H|T], N),
	N > floor(Z), 
	N < ceiling(Z).

