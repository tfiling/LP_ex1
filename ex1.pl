%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Task 1 - percentage

countCandG('C').
countCandG('G').

% checks if N is an even number
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
% Task 2 - hamming distance

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

% a case where the heads is identical
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
    count(N, Word1, Word2),         % make sure both words are of length N
    Z is N / 2,                     
    innerHamming(Z, Word1, Word2).  % look for at least N / 2 chars that are different


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% task 3 - reverse_complement

% reverse(X, Y, Z) written in mode (+, -, +) - reverses X and places the result in Y
% reverse was copied from https://stackoverflow.com/questions/19471778/reversing-a-list-in-prolog
% reverse function's base case
reverse([],Z,Z).

% push back the head to the accumulator
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

% replace(A, B, C, D) in mode (+, +, +, -) replaces any occurance of A with B and B with A in list C
% when impementing replace, took insparation from https://stackoverflow.com/questions/5850937/prolog-element-in-lists-replacement
% base case - empty lists apply the replacement
replace(_, _, [], []).

% the the first element in the first list is a replacment of the one in the second list and vice versa
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [R|T], [O|T2]) :- replace(O, R, T, T2).

% do not replace if the current head elements are not the onse the should be replace
replace(O, R, [H|T], [H|T2]) :- H \= O, H \= R, replace(O, R, T, T2).

% apply the replacement required for the transition from W to W^C
replaceWord(Word, WordC) :-
    replace('A', 'T', Word, Tmp),   % 1st step - relpace all 'A' with 'T' and vice versa
    replace('C', 'G', Tmp, WordC).  % 2nd step - replace all 'C' with 'G' and vice versa, where the result would be WordC


reverse_complement(N, Word1, Word2) :-
    reverse(Word1, Word1R, []),     % reverse Word1 to become w1^R
    replaceWord(Word2, Word2C),     % apply the replacements converting Word2 to become W2^C
    hamming(N, Word1R, Word2C).     % check for hamming distance as part of reverse complement's definition


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Task 4 - is_dna (helper function)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkAllForPercentage(A, B) with mode (+, +) - A is the length of all words in B, true if all words fulfill percentage

% base case
checkAllForPercentage(_, []).

% recursive list iterator checking for percentage on every word in the list
checkAllForPercentage(N, [H | T]) :-
    percentage(N, H),
    checkAllForPercentage(N, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkAllForHammingDistance(A, B) with mode (+, +) - A is the length of all words in B, true if all possible pairs of words fulfill hamming distance property

% IMPORTANT - additional explenation in the attached pdf file
% base cases
checkAllForHammingDistance(_, _, []).

% check hamming distance between H1 and all of the elements in the list
checkAllForHammingDistance(N, H1, [H2 | T]) :-
    hamming(N, H1, H2),                     % actual hamming comparison
    checkAllForHammingDistance(N, H1, T).   % recursive call to apply the comparison between H1 and the rest of the elements in the list

checkAllForHammingDistance(_, []).
checkAllForHammingDistance(N, [H | T]) :-
    checkAllForHammingDistance(N, H, T),    % check hamming distance H and all of the elements in the tail T
    checkAllForHammingDistance(N, T).       % recursivly apply between each element in the tail and those that follow it in the list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checkAllForReverseComplement(A, B) with mode (+, +) - A is the length of all words in B, true if all possible pairs of words fulfill reverse compellant property

% base cases
checkAllForReverseComplement(_, _, []).

% check hamming distance between H1 and all of the elements in the list
checkAllForReverseComplement(N, H1, [H2 | T]) :-
    reverse_complement(N, H1, H2),
    checkAllForReverseComplement(N, H1, T).

checkAllForReverseComplement(_, []).
checkAllForReverseComplement(N, [H | T]) :-
    checkAllForReverseComplement(N, H, T),    % check hamming distance H and all of the elements in the tail T
    checkAllForReverseComplement(N, T).       % recursivly apply between each element in the tail and those that follow it in the list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Task 4 - is_dna


is_dna(N, M, Words) :-
    count(Words, M),                        % verify M lists
    checkAllForPercentage(N, Words),
    checkAllForHammingDistance(N, Words),
    checkAllForReverseComplement(N, Words), % checks only half of the combinations since every word be W1 and W2 (the operation is not associative)
    reverse(Words, WordsR, []),             % reverse Words
    checkAllForReverseComplement(N, WordsR).% check the other half of Words

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% task 5 - dna_word

 dna_word(N, Word) :-
     length(Word, N),           % make Word be of length N
     possible_dna_word(Word).   

 possible_dna_word([X|Xs]) :-
     member(X, ['A','C','G','T']),  % set the first char to be one of the DNA chars
     possible_dna_word(Xs).         % apply recursivly for the rest of the Word
 possible_dna_word([]).             % case base



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Task 6 - random_dna_word

random_dna_word(N, Word) :-
    list_of_all_acgt_permutations(N, AllDNAWords),  % get all possible permutations
    random_permutation(AllDNAWords, RandomOrder),   % generate a random order for the list containing all all possible permutations of DNA words
    member(Word, RandomOrder).                      % set Word to be the first word in the random order DNA words list

% a function with mode (+N, -AllPerms) - returns all possible permutations of of length N DNA words
list_of_all_acgt_permutations(N, AllPerms) :-
    list_of_all_acgt_permutations(N, [['A'], ['C'], ['G'], ['T']], [], AllPerms).

% append all 4 possible chars ,which creates 4 new lists.
list_of_all_acgt_permutations(N, [H|T], BuildList, AllPerms) :-
    N > 1,
    append(['A'], H, L1),
    append(['C'], H, L2),
    append(['G'], H, L3),
    append(['T'], H, L4), %create all possible combinations of ACGT words
    list_of_all_acgt_permutations(N, T, [L1, L2, L3, L4|BuildList], AllPerms). %add created combinations to list already built and keep applying recursively on the tail of posible permutations (T)

list_of_all_acgt_permutations(N, [], BuildList, AllPerms) :-  %finished going throw all chars in [['A'], ['C'], ['G'], ['T']]
    N > 1,
    N1 is N - 1, %need this inorder to get all of the combinations of DNA words for length N
    list_of_all_acgt_permutations(N1, BuildList, [], AllPerms).

%stop condition when the above function reaches N=1
list_of_all_acgt_permutations(N, FinishedList, [], AllPerms) :-  
    N = 1, %DNA words are now of length N that we received from the input of the function random_dna_word 
    FinishedList = AllPerms.


%%%%%%%%%%%%%%%%%%
% task 7 - increment

increment(Words, Word, N) :-
    count(Words, Count),                    % count how many words in Words
    NewCount is Count + 1,                  % increament the above count
    is_dna(N, NewCount, [Word | Words]).    % check if the new list created from appending Word fulfills the is_dna property

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Task 8 - dna

dna(N, M, Words) :-
    list_of_all_acgt_permutations(N, AllPerms), % generate all possible words of length N
    appendDnaWords(N, M, AllPerms, [], Words).  % append togather M of those words as long as each newly appended word dosnt break the fulfillment of is_dna by the existing list

% (+N, +M, +List, +Acc, -Words) - iterates the words in List and on every iteration appends the first word, 
% to Acc if and only if it wont break the fulfillment of is_dna property by Acc.
% in total M words will be added this way
appendDnaWords(N, M, [H | T], Acc, Words) :-
    M > 0,                  % make sure a new word should be added
    increment(Acc, H, N),   % add the word, true if the word added successfully while keeping the fulfilment of is_dna property
    M2 is M - 1,            
    appendDnaWords(N, M2, T, [H | Acc], Words). % a word was added so apply the recursive call with the new accumulator and M-1 words left to be added

appendDnaWords(N, M, [H | T], Acc, Words) :-
    M > 0,
    \+increment(Acc, H, N),             % the new word breaks the is_dna predicate and therefor wont be added to the list
    appendDnaWords(N, M, T, Acc, Words).% recusively try the next word

appendDnaWords(_, 0, _, Words, Words).  % base case for the function, setting the accumulator to be the resulted Words

appendDnaWords(_, 0, _, Words, _) :-
    writeln(Words),
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Task 9 - random_dna

random_dna(N, M, Words) :- 
    list_of_all_acgt_permutations(N, AllPerms),             % generate all possible Words of length
    random_permutation(AllPerms, AllPermsRandomOrder),      % shuffle the order of the resulted AllPerms word list
    appendDnaWords(N, M, AllPermsRandomOrder, [], Words).   % use the helper function from Task 8 to append the words in the shuffled order 
                                                            % - this will result a different Word list on every invocation of random_dna

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Task 10

dna_subset(N, Words, Subset) :-
    dna_subset(N, Words, [], Subset).

<<<<<<< HEAD
dna_subset(N, [H|T], Acc, Subset) :-
    increment(Acc, H, N),
    dna_subset(N, T, [H|Acc], Subset).  %if can insert Word to Acc, then insert it to Acc

dna_subset(N, [_|T], Acc, Subset) :-  %case were can't insert Word to Acc
    dna_subset(N, T, Acc, Subset).

dna_subset(_, [], Acc, Subset) :-
    Acc = Subset.
=======

% % base case
% dna_subset(N, Words, Words) :-
%     Count(M, Words),
%     is_dna(N, M, Words).
>>>>>>> d3844a59f4f551bd2004774b990d7e0b1f076a90
