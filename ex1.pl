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



% gal: proposal for count function that counts the size of a list
count([], 0).
count([_|T], N) :-
	count(T, Y),
    N is Y+1,
    N > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hamming distance

% true if both words has length of N
count(N, Word1, Word2) :-
    count(Word1, N),
    count(Word2, N).

% end of recursion - anything has hamming distance of at lease 0
innerHamming(0, _, _).

% a case where the head is identical
innerHamming(N, [H1 | T1], [H2 | T2]) :-
    H1 == H2,
    innerHamming(N, T1, T2).

% a case where the head is not identical
innerHamming(N, [H1 | T1], [H2 | T2]) :-
    H1 \== H2,
    Z is N - 1,
    innerHamming(Z, T1, T2).

hamming(N, Word1, Word2) :-
    count(N, Word1, Word2),
    Z is N / 2,
    innerHamming(Z, Word1, Word2).


