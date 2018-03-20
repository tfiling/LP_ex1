%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% percentage

countCandG('C').
countCandG('G').

isEven(N) :-
    A is ceiling(N / 2),
    B is floor(N / 2),
    A == B. % both floor an ceiling returned the same value on division by 2 -> N modulo 2 is 0 and N is even

% base case
percentageInner(0, []).

% head is C or G
percentageInner(N, [H | T]) :-
    countCandG(H),          % head is C/G
    Y is N - 1,             % counted one more C/G, decrease the C/G counter
    percentageInner(Y, T).  % keep counting C/G on the list's tail

% head is not C or G
percentageInner(N, [H | T]) :-
    \+ countCandG(H),       % head is not C/G
    percentageInner(N, T).  % keep counting in the tail

% attend floor of the n / 2 division 
 percentageInnerFloorN(N, Word) :-
    Floor is floor(N / 2),          % calculate the floor of N / 2
    percentageInner(Floor, Word).   % true if Word has Floor appearances of C/G

% attend Ceiling of the n / 2 division
 percentageInnerCeilingN(N, Word) :-
    Ceiling is ceiling(N / 2),      % calculate the ceiling of N / 2
    percentageInner(Ceiling, Word). % true if word has Ceiling appearances of C/G

% since N is odd, floor(N/2) \== ceiling(N/2) and we should check if true for both
% important note - [floor(N/2), ceiling(N/2)] can have either 1 or two integers
% check percentage for floor(N/2)
percentageInnerNOdd(N, Word) :-
    percentageInnerFloorN(N, Word).

% check percentage for ceiling(N/2)
percentageInnerNOdd(N, Word) :-
    percentageInnerCeilingN(N, Word).

percentage(N, Word) :-
    isEven(N),                  % true if N is even
    Z is N / 2,                 % calculate N / 2 (result is integer)
    percentageInner(Z, Word).   % check percentage (has only one value to check)

percentage(N, Word) :-
    \+isEven(N),                    % true if N is odd
    percentageInnerNOdd(N, Word).   % check pecentage for both floor(N/2) and ceiling(N/2)
                                    % by overloading percentageInnerNOdd - can be true only for one of the values

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hamming distance

% count elements in the list
% base case - empty list
count([], 0).
% increase counter and continue counting the list's tail
count([_|T], N) :-
	count(T, Y),
    N is Y+1,
    N > 0.


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

