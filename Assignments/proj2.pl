% You can use this code to get started with your fillin puzzle solver.
% Make sure you replace this comment with your own documentation.
:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).

solve_puzzle(Puzzle, WordList, SolvedPuzzle) :-
        init_variables(Puzzle, VarPuzzle),
        init_slots(VarPuzzle, Slots),
        sort_words_slots(WordList, Slots, Pairs),
        insert_words(Pairs),
        SolvedPuzzle = VarPuzzle.

% ------------------------------------------------ %
% Start Initialisation Functions 
% ------------------------------------------------ %  
	
% Initialise the puzzle with variables in order to make unification easier
% later on in the processing. Uses an NewPuzzle & Prev to accumulate results.
init_variables(OldPuzzle, NewPuzzle) :-
	init_variables(OldPuzzle, [], NewPuzzle).
init_variables([], NewPuzzle, NewPuzzle).
init_variables([Row|Rows], Prev, NewPuzzle) :-
	init_vars_row(Row, [], Acc1),
        append(Prev, [Acc1], Acc2),
	init_variables(Rows, Acc2, NewPuzzle).

% Initialises the rows of the puzzle, uses NewRow & Prev to accumulator 
% results and newRow returns the results.
init_vars_row([], NewRow, NewRow).
init_vars_row([X|Xs], Prev, NewRow) :-
	(  X = '_'
	-> A = _
	;  % else we have a letter or hashtag
	   A = X
	),
        append(Prev, [A], Acc),
	init_vars_row(Xs, Acc, NewRow).

% Initialises the slots needed for unification using the variable puzzle
% created in init_variables. Transpose the puzzle to avoid writing extra
% code to process the columns.
init_slots(Puzzle, Slots) :-
	init_slots(Puzzle, [], HorizontalSlots),
	transpose(Puzzle, VertPuzzle),
	init_slots(VertPuzzle, [], VerticalSlots),
	append(HorizontalSlots, VerticalSlots, Slots).
init_slots([], Slots, Slots).
init_slots([Row|Rows], Prev, Slots) :-
	init_slots_row(Row, [], [], NewSlots),
	append(Prev, NewSlots, Acc2),
	init_slots(Rows, Acc2, Slots).

% Looks for slots greater than size 1 which indicates a word can be stored.
% Stores the accumulating slot in AccSlot, Stores those complete slots in Prev
init_slots_row([], Prev, AccSlot, Slots) :-
	% Reached the end of the row
	length(AccSlot, Len),
	(  Len > 1
	-> length(Prev, PrevLen),
	   % Deal with Prev being empty to avoid returning a list instead of
	   % a list of lists.
	   (  PrevLen > 0
	   -> append(Prev,[AccSlot], Slots)
	   ;  % else prev is 0
	      Slots = [AccSlot]
	   )
	;  % else no more slots to address
	   Slots = Prev
	).
init_slots_row([X|Xs], Prev, AccSlot, Slots) :-
	length(AccSlot, Len),
	(  (nonvar(X), X = '#')
	-> (  Len > 1
	   -> append(Prev, [AccSlot], Acc2),
              init_slots_row(Xs, Acc2, [], Slots)
	   ;  init_slots_row(Xs, Prev, [], Slots)
	   )
	;  % else X is a character or blank
           append(AccSlot, [X], AccSlot2),
	   init_slots_row(Xs, Prev, AccSlot2, Slots)
	).
	
test_init(PuzzleFile, Output) :-
	read_file(PuzzleFile, Puzzle),
	init_variables(Puzzle, VarPuzzle),
	init_slots(VarPuzzle, Output).
	   
% ------------------------------------------------ %  
% End Initialisation Functions
% ------------------------------------------------ %  
	
% ------------------------------------------------ %
% Start Sorting Functions 
% ------------------------------------------------ %  

word_quicksort_mid_pivot(Xs,Ys):-
        middle(Xs, X),
        partition1(Xs, X, Left, Right),
        quicksort1(Left, Ls),
        quicksort1(Right, Rs),
        append(Ls, Rs, Ys).
word_quicksort_mid_pivot([],[]).

quicksort1([X|Xs],Ys) :-
        partition1(Xs,X,Left,Right),
        quicksort1(Left,Ls),
        quicksort1(Right,Rs),
        append(Ls,[X|Rs],Ys).
quicksort1([],[]).

% Partitioning key value pairs eg. [a]-[_]
partition1([X|Xs],Y,[X|Ls],Rs) :-
        length(Y, Y_Len),
        length(X, X_Len),
        X_Len =< Y_Len,
        partition1(Xs,Y,Ls,Rs).
partition1([X|Xs],Y,Ls,[X|Rs]) :-
        length(Y, Y_Len),
        length(X, X_Len),
        X_Len > Y_Len,
        partition1(Xs,Y,Ls,Rs).
partition1([],_,[],[]).

sort_words_slots(WordList, SlotList, SortedPairs) :-
        quicksort1(SlotList, SortedSlots),
        word_quicksort_mid_pivot(WordList, SortedWords),
        pack(SortedSlots, PackedSlots),
        pack(SortedWords, PackedWords),
        pair_words_with_slots(PackedWords, PackedSlots, Pairs),
        quicksort_mid_pivot(Pairs, SortedPairs).

% Zip into Pairormat: [[Words of Same Length]-[Slots of Same Length]]
zip_into_pair([], [], []).
zip_into_pair([X|Xs], [Y|Ys], [X-Y|Zs]) :-
        zip_into_pair(Xs, Ys, Zs).

pair_words_with_slots(PackedWords, PackedSlots, SortedPairs) :-
        pair_words_with_slots(PackedWords, PackedSlots, [], SortedPairs).
pair_words_with_slots([],_, FinalPairs, FinalPairs).
pair_words_with_slots([WGroup|WGroups],[SGroup|SGroups], Acc, FinalPairs):-
        pair_a_word_with_slots(WGroup, SGroup, [], Pairs),
        append(Pairs,Acc,Acc2),
        pair_words_with_slots(WGroups, SGroups, Acc2, FinalPairs).

pair_a_word_with_slots([],_,Pairs, Pairs).
pair_a_word_with_slots([Word|Words],Slots, Acc, Pairs):-
        test_word(Word, Slots, UnifiableList),
        pair_a_word_with_slots(Words, Slots, [Word-UnifiableList|Acc], Pairs).

test_word(Word, Slots, UnifiableList):-
        test_word(Word, Slots, [], UnifiableList).
test_word(_,[],Slots,Slots).
test_word(Word, [Slot|Slots], Ys, List):-
        unifiable(Word, Slot, _),
        test_word(Word, Slots, [Slot|Ys], List).
test_word(Word, [Slot|Slots], Ys, List):-
        not(unifiable(Word, Slot, _)),
        test_word(Word, Slots, Ys, List).


% Pack function adapted from
% http://www.ic.unicamp.br/~meidanis/courses/problemas-prolog/p09.prolog
% Packs repeated lengths of elements into sublists.
pack([],[]).
pack([X|Xs], [Z|Zs]) :-
        transfer(X,Xs,Ys,Z),
        pack(Ys,Zs).

% transfer(X,Xs,Ys,Z) Ys is the list that remains from the list Xs
%    when all leading copies of length X are removed and transfered to Z
transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :-
        length(X, Len_X),
        length(Y, Len_Y),
        dif(Len_X,Len_Y).
transfer(W, [X|Xs], Ys, [W|Zs]) :-
        length(W, Len_W),
        length(X, Len_X),
        Len_W =:= Len_X,
        transfer(X, Xs, Ys, Zs).

% Insertion Sort adapted From http://www.cs.princeton.edu/courses/archive/
% spr11/cos333/lectures/17paradigms/sort.prolog 

insertionSort([], []).
insertionSort([Head|Tail], Result) :-
        insertionSort(Tail, List), insertInPlace(Head, List, Result).

insertInPlace(Element, [], [Element]).
insertInPlace(Element, [Head|Tail], [Element|List]) :-
        length(Element, El_len),
        length(Head, Head_len),
        El_len =< Head_len,
        insertInPlace(Head, Tail, List).
insertInPlace(Element, [Head|Tail], [Head|List]) :-
        length(Element, El_len),
        length(Head, Head_len),
        El_len > Head_len,
        insertInPlace(Element, Tail, List).

% Used to grab the middle element for pivot selection in
% the quicksort function.
middle(List, Middle) :-
    middle(List, List, Middle).
middle([M|_], [_,_], M).
middle([M|_], [_], M).
middle([_|T], [_,_,X|T2], Middle) :-
    middle(T, [X|T2], Middle).

% Code adapted from https://www.cp.eng.chula.ac.th/
% ~piak/teaching/dsys/2004/quick-prolog.htm
quicksort_mid_pivot(Xs,Ys):-
        middle(Xs, X),
        partition(Xs, X, Left, Right),
        quicksort(Left, Ls),
        quicksort(Right, Rs),
        append(Ls, Rs, Ys).
quicksort_mid_pivot([],[]).

quicksort([X|Xs],Ys) :-
        partition(Xs,X,Left,Right),
        quicksort(Left,Ls),
        quicksort(Right,Rs),
        append(Ls,[X|Rs],Ys).
quicksort([],[]).

% Partitioning key value pairs eg. [a]-[_]
partition([Xk-Xv|Xs],Yk-Yv,[Xk-Xv|Ls],Rs) :-
        length(Yv, Y_Len),
        length(Xv, X_Len),
        X_Len =< Y_Len,
        partition(Xs,Yk-Yv,Ls,Rs).
partition([Xk-Xv|Xs],Yk-Yv,Ls,[Xk-Xv|Rs]) :-
        length(Yv, Y_Len),
        length(Xv, X_Len),
        X_Len > Y_Len,
        partition(Xs,Yk-Yv,Ls,Rs).
partition([],_,[],[]).

% ------------------------------------------------ %
% End Sorting Functions 
% ------------------------------------------------ %  

% ------------------------------------------------ %
% Start Insertion Functions
% ------------------------------------------------ %

test_solve(PuzzleFile, WordFile, Output) :-
        read_file(PuzzleFile, Puzzle),
        read_file(WordFile, WordList),
        init_variables(Puzzle, VarPuzzle),
        init_slots(VarPuzzle, Slots),
        sort_words_slots(WordList, Slots, Pairs),
        insert_words(Pairs),
        Output = VarPuzzle.

pairinsertionSort([], []).

pairinsertionSort([HEAD|TAIL], RESULT) :-
   pairinsertionSort(TAIL, LIST), pairinsertInPlace(HEAD, LIST, RESULT).

%-----------------------------------------------------------------------
% insertInPlace(ELEMENT, LIST, RESULT)
% RESULT is the result of inserting ELEMENT into sorted list LIST
% at the correct place.
%-----------------------------------------------------------------------

pairinsertInPlace(ELEMENT, [], [ELEMENT]).
pairinsertInPlace(Word-Slots, [Word2-Slots2|TAIL], [Word-Slots|LIST]) :-
        length(Slots, Slots_Len),
        length(Slots2, Slots2_Len),
        Slots_Len =< Slots2_Len, pairinsertInPlace(Word2-Slots2, TAIL, LIST).

pairinsertInPlace(Word-Slots, [Word2-Slots2|TAIL], [Word2-Slots2|LIST]) :-
        length(Slots, Slots_Len),
        length(Slots2, Slots2_Len),
        Slots_Len > Slots2_Len, pairinsertInPlace(Word-Slots, TAIL, LIST).

% Pair is in Key Value Pair Format eg. [Word]-[[Slot]]
insert_words([]).
insert_words([Word-[Slot|Slots]|Pairs]):-
        length([Slot|Slots],SlotLength),
        SlotLength > 0, % This ensures a quick fail.
        (  (Word = Slot, prune_pairs(Pairs) ) 
        % Successful Unification
        -> true  % Do absolutely nothing
        ; % Failed Unification, Try and test it on the other compatible slots.
           length(Slots, RemainingLen),
           RemainingLen > 0,
           insert_words([Word-Slots|Pairs])
        ).
        

% Since we've unified a slot with a word, we have to remove this slot from
% the other pairs.
prune_pairs(Pairs):-
        prune_pairs(Pairs, []).
prune_pairs([],Pruned) :-
        quicksort_mid_pivot(Pruned, SortedPruned),
        insert_words(SortedPruned).
prune_pairs([Word-Slots|Pairs], Acc):-
        test_word(Word, Slots, Remaining),
        length(Remaining, NumRemaining),
        NumRemaining > 0,
        prune_pairs(Pairs, [Word-Remaining|Acc]).
% ------------------------------------------------ %
% End Insertion Functions 
% ------------------------------------------------ %  