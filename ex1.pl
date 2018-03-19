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

% base case - anything has hamming distance of at lease 0
innerHamming(0, _, _).

% a case where the head is identical
innerHamming(N, [H1 | T1], [H2 | T2]) :-
    H1 == H2,
    innerHamming(N, T1, T2).

% a case where the head is not identical
innerHamming(N, [H1 | T1], [H2 | T2]) :-
    H1 \== H2,
    Z is N - 1,                 
    innerHamming(Z, T1, T2).    % found one char that not equal - find out if there are other N - 1 non identical chars

% the actual function
hamming(N, Word1, Word2) :-
    count(N, Word1, Word2),
    Z is N / 2,                     
    innerHamming(Z, Word1, Word2).  % look for at least N / 2 chars that are different


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reverse_complement

% copied from https://stackoverflow.com/questions/19471778/reversing-a-list-in-prolog
% reverse function's base case
reverse([],Z,Z).

% push back the head to the accumulator
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

% took insparation from https://stackoverflow.com/questions/5850937/prolog-element-in-lists-replacement
% base case - empty lists apply the replacement
replace(_, _, [], []).

% the the first element in the first list is a replacment of the one in the second list and vice versa
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [R|T], [O|T2]) :- replace(O, R, T, T2).

% do not replace if the current head elements are not the onse the should be replace
replace(O, R, [H|T], [H|T2]) :- H \= O, H \= R, replace(O, R, T, T2).

% apply the replacement required for the transition from W to W^C
replaceWord(Word, WordC) :-
    replace('A', 'T', Word, Tmp),
    replace('C', 'G', Tmp, WordC).


reverse_complement(N, Word1, Word2) :-
    reverse(Word1, Word1R, []),     % reverse Word1 to become w1^R
    replaceWord(Word2, Word2C),     % apply the replacements converting Word2 to become W2^C
    hamming(N, Word1R, Word2C).     % check for hamming distance as part of reverse complement's definition

