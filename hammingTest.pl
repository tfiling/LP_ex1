% name of the tested file without the pl extension
[ex1].

% test predicate
test(Summary, Predicate) :-
  string_concat('\n ', Summary, Buf),
  string_concat(Buf, ' is running', Message),
  write(Message),
  call(Predicate),
  write('\ndone.').

test :-
    test('empty words', (
        hamming(0, [], []),
        \+hamming(1, [], [])
    )),

    test('words in the same length', (
        hamming(_, [a], [a]),
        hamming(_, [a, b], [a, b]),
        \+hamming(_, [a], [a, b])
    )).

% run tests on compile
:- test.