%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% count tests - an example template

:- include('ex1.pl'). % import the tested script

test(1, [], 0).     % args: (index, arg1, arg2)
test(2, ['A'], 1).

% test count
tdd :-
    test(I, W1, D1),    
    count(W1, D2),
    (D1=D2 -> writeln(I: ok);writeln('failed')),
    fail.
tdd.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkAllForPercentage tests

test1(1, 0, []).
test1(2, 8, [['A','A','A','A','C','G','C','G']]).
test1(3, 7, [['A','A','A','A','C','G','C']]).
test1(4, 8, [['A','A','A','A','C','G','C','G'], ['A','A','A','A','C','G','C','G']]).
test1(5, 7, [['A','A','A','A','C','G','C'], ['A','A','A','A','C','G','C']]).
test1(6, 8, [['A','A','A','A','C','G','C','G'], ['A','A','A','A','C','G','C','G'], ['A','A','A','A','C','G','C','G']]).
test1(7, 7, [['A','A','A','A','C','G','C'], ['A','A','A','A','C','G','C'], ['A','A','A','A','C','G','C']]).

testCheckAllForPercentage :-
    test1(I, N, Ws),
    (checkAllForPercentage(N, Ws) -> writeln(I: ok);writeln(I: failed)),
    fail.
testCheckAllForPercentage.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkAllForReverseComplement tests

test2(1, 8, [['A','A','A','A','C','C','C','C'], ['A','A','A','A','C','C','C','G']]).

testCheckAllForReverseComplement :-
    test2(I, N, Ws),
    (checkAllForReverseComplement(N, Ws) -> writeln(I: ok);writeln(I: failed)),
    fail.
testCheckAllForReverseComplement.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_dna tests

test3(1, 8, 2, [['T','T','G','A','G','T','C','C'], ['A','G','T','T','T','G','C','G']]).

testIs_dna :-
    test3(I, N, M, Ws),
    (is_dna(N, M, Ws) -> writeln(I: ok);writeln(I: failed)),
    fail.
testIs_dna.


:- testCheckAllForPercentage, testCheckAllForReverseComplement, testIs_dna.