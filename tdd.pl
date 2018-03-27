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

test3(1, 8, 2, [['T','T','G','A','G','T','C','C'], ['A','G','T','T','T','G','C','G']], 1).
test3(2, 8, 4, [['C','C','C','T','G','T','A','A'],
                ['C','C','C','T','A','A','T','G'],
                ['T','C','G','T','A','C','A','C'],
                ['T','T','C','G','G','A','G','T']], 1).
test3(3, 8, 6, [['T','T','G','A','G','T','C','C'],
                ['A','G','T','T','T','G','C','G'],
                ['G','T','G','T','C','G','T','G'],
                ['T','C','T','T','C','G','T','G'],
                ['A','A','A','G','C','C','C','T'],
                ['T','T','A','T','G','G','G','G']], 0).

testIs_dna :-
    test3(I, N, M, Ws, ShouldPass),
    (ShouldPass is 1 -> 
        (is_dna(N, M, Ws) -> writeln(I: ok);writeln(I: failed)) ;
        (is_dna(N, M, Ws) -> writeln(I: failed); writeln(I: ok))
    ),
    fail.
testIs_dna.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% increment tests

test4(1,    [['G','G','T','G','A','A','T','G'],
            ['G','A','A','C','A','T','G','C'],
            ['C','C','C','T','A','C','A','T']],

            ['T','A','T','C','A','G','G','G'],
             8, 
             1).

test4(2,    [['T','T','G','A','G','T','C','C'],
            ['A','G','T','T','T','G','C','G'],
            ['G','T','G','T','C','G','T','G'],
            ['A','A','A','G','C','C','C','T'],
            ['T','T','A','T','G','G','G','G']],

             ['T','C','T','T','C','G','T','G'],
             8, 
             0).

testIncrement :-
    test4(I, Ws, W, N, ShouldPass),
    (ShouldPass is 1 -> 
        (increment(Ws, W, N) -> writeln(I: ok);writeln(I: failed)) ;
        (increment(Ws, W, N) -> writeln(I: failed); writeln(I: ok))
    ),
    fail.
testIncrement.





:- testCheckAllForPercentage, testCheckAllForReverseComplement, testIs_dna, testIncrement.