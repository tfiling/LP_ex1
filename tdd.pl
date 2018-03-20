% % count elements in the list
% % base case - empty list
% count([], 0).
% % increase counter and continue counting the list's tail
% count([_|T], N) :-
% 	count(T, Y),
%     N is Y+1,
%     N > 0.

:- include('ex1.pl').

test(1, [], 0).
test(2, ['A'], 1).

% test count
tdd :-
    test(I, W1, D1),
    count(W1, D2),
    (D1=D2 -> writeln(I);writeln('failed')),
    fail.
tdd.