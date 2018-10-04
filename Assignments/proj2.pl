% Declarative Programming COMP30020 Project 2
% By Sam Chung - Chungs1 - 758 053
% Semester 2 Unimelb 2018

% Project Explanation:
% The project was written for a University of Melbourne subject. The aim of 
% this project is to write prolog code in order to solve a crossword like 
% puzzle called a fill-in puzzle. A list of words is provided alongside a 
% crossword puzzle and the objective is to fill this puzzle with the words
% in a manner such that all the columns and rows make up the words.
% A more in-depth explanation of what a fill in puzzle is can be found here: 
% https://en.wikipedia.org/wiki/Fill-In_(puzzle).

% The aim of this project was to create efficient code which will solve puzzles
% of different sizes and complexities in under 20 seconds as tested by the
% university on linux servers. If we wanted to do it extremely inefficiently,
% we could test every single combination however for example: a 7 × 7 puzzle 
% which may have 18 words can have 18! > 1015 permutations which would take an
% extremely long time to process. Hence there are a number of efforts made to 
% optimise the efficiency of the code in place of simplicity.

% Terminology Used:
% Pair : Pairs are used in SWI-Prolog in the format of key-value
% Slot : A slot is a space which a word can fit into. eg. [a,b,c] can fit
%        into a slot [_,_,_].

% Ensures the correct transpose predicate is loaded as SWI-Prolog loads
% an incompatible version by default.
:- ensure_loaded(library(clpfd)).

% ------------------------------------------------ %
% Start File Read-in Functions
% (Provided by the University)
% ------------------------------------------------ %  

% Reads the Puzzle File and Word File, Validates, Solves
% the Maze and Prints it to the SolutionFile.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

% Reads all characters in a file into a stream and passes that to read_lines.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% Interprets a stream and separates that into lines to pass to read_line.
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

% Interprets lines into individual characters and parses these into lists.
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

% Takes Puzzle which takes the form of a list of character lists and 
% parses it into SolutionFile.
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% Maps put_puzzle_char over rows.
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% Converts the character back into a stream.
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% Validates a puzzle
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).

% Solves a puzzle by:
% 1. Converting characters into variables (eg. '_' into free variables).
% 2. Creating slots where words can fit into in order to enable unification.
% 3. Creating pairs which store Words with compatible slots. These are sorted
%       to minimise the search space when we try to unify the words with slots.
% 4. Unification by inserting words into slots.
% 5. Returning the solve puzzle back to the main predicate.
solve_puzzle(Puzzle, WordList, SolvedPuzzle) :-
        % Preparing the Puzzle for Solving
        init_variables(Puzzle, VarPuzzle),
        create_slots(VarPuzzle, Slots),
        % Optimising the Puzzle for Solving
        create_pairs(WordList, Slots, Pairs),
        % Solving the Puzzle
        insert_words(Pairs),
        SolvedPuzzle = VarPuzzle.

% ------------------------------------------------ %
% End File Read-in Functions
% ------------------------------------------------ % 

% ------------------------------------------------ %
% Start Preparation Functions 
% ------------------------------------------------ %  
	
% Iterates over the puzzle and calls init_vars_row in order to process over
% rows. Uses accumulators in order to store completed rows. The purpose of this
% predicate is to replace '_' with free variables in order to enable unification
% in prolog.
init_variables(OldPuzzle, NewPuzzle) :-
	init_variables(OldPuzzle, [], NewPuzzle).
init_variables([], NewPuzzle, NewPuzzle).
init_variables([Row|Rows], Prev, NewPuzzle) :-
	init_vars_row(Row, [], NewRow),
        append(Prev, [NewRow], Acc),
	init_variables(Rows, Acc, NewPuzzle).

% Changes '_' into free variables, Otherwise leaves them as is. Uses
% an accumulator to store the processed letters.
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
% created in init_variables. Transposes the puzzle to avoid writing extra
% code to process the columns. Processes the var puzzle row by row and calls
% create_slots_row to process the row.
create_slots(Puzzle, Slots) :-
	create_slots(Puzzle, [], HorizontalSlots),
	transpose(Puzzle, VertPuzzle),
	create_slots(VertPuzzle, [], VerticalSlots),
	append(HorizontalSlots, VerticalSlots, Slots).
create_slots([], Slots, Slots).
create_slots([Row|Rows], Prev, Slots) :-
	create_slots_row(Row, [], [], NewSlots),
	append(Prev, NewSlots, Acc2),
	create_slots(Rows, Acc2, Slots).

% Looks for slots greater than size 1 which indicates a word can be stored.
% This is fairly complex as there are a number of cases to consider. A slot can
% be found if:
% Condition 1: The space size is > 1 (We have to ignore single _'s)
% Condition 2: The character # is found while Condition 1 is satisfied.
% Condition 3: We reach the end of the line while Condition 1 is satisfied.
% Stores the accumulating space size in Space_Size, 
% Stores those complete slots in Prev
create_slots_row([], Prev, Space_Size, Slots) :-
	length(Space_Size, Len),
	(  Len > 1
	-> length(Prev, PrevLen),
	   % Deal with Prev being empty to avoid returning a list instead of
	   % a list of lists.
	   (  PrevLen > 0
	   -> append(Prev,[Space_Size], Slots)
	   ;  % else prev is 0
	      Slots = [Space_Size]
	   )
	;  % else no more slots to address
	   Slots = Prev
	).
create_slots_row([X|Xs], Prev, Space_Size, Slots) :-
	length(Space_Size, Len),
        % nonvar ensures that the free variable doesn't get unified with #
	(  (nonvar(X), X = '#')
	-> (  Len > 1
	   -> append(Prev, [Space_Size], Acc2),
              create_slots_row(Xs, Acc2, [], Slots)
	   ;  create_slots_row(Xs, Prev, [], Slots)
	   )
        % else X is a character or blank, just add it to the space size
        % accumulator and keep going until we hit a base case.
	;
           append(Space_Size, [X], Space_Size2),
	   create_slots_row(Xs, Prev, Space_Size2, Slots)
	).
	   
% ------------------------------------------------ %  
% End Preparation Functions
% ------------------------------------------------ %  
	
% ------------------------------------------------ %
% Start Optimisation Functions 
% ------------------------------------------------ %  

% Converts a list of words and a list of slots into pairs of format
% Word-[[Slot]] which represent a word and compatible slots which it fits.
%
% 1. Sort Wordlist & Slotlists by length
% 2. Pack all items in both of the lists into lists of same length 
%        eg. [[a,b],[c,d]],[[a,b,c],[d,e,f]] (Words)
%           [[_,_],[_,_]],[[_,_,_],[_,_,_]] (Slots)
% 3. Run a unifiability predicate on them and put them in pairs
%    of format [Word-[UnifiableSlots]], where Word is a single word and 
%    UnifiableSlots is a list of compatible slots.
% 4. Sort the pairs by the length of UnifiableSlots. This minimises
%        our search space when we try to solve the crossword.
create_pairs(WordList, SlotList, SortedPairs) :-
        % Sorting Lists
        quicksort_single(SlotList, SortedSlots),
        quicksort_single_mid_pivot(WordList, SortedWords),
        % Packing Items of Same Length
        pack(SortedSlots, PackedSlots),
        pack(SortedWords, PackedWords),
        % Pairing with unifiability predicate
        pair_words_with_slots(PackedWords, PackedSlots, Pairs),
        % Sorting Again
        quicksort_pair_mid_pivot(Pairs, SortedPairs).

% Takes the packedlists and passes 1 of each to pair_words_with_slots to
% process. Accumulates the Pairs in FinalPairs and returns that back.
pair_words_with_slots(PackedWords, PackedSlots, SortedPairs) :-
        pair_words_with_slots(PackedWords, PackedSlots, [], SortedPairs).
pair_words_with_slots([],_, FinalPairs, FinalPairs).
pair_words_with_slots([WGroup|WGroups],[SGroup|SGroups], Acc, FinalPairs):-
        pair_a_word_with_slots(WGroup, SGroup, [], Pairs),
        append(Pairs,Acc,Acc2),
        pair_words_with_slots(WGroups, SGroups, Acc2, FinalPairs).

% Simply assumes that the packedlists are of the same length, passes it to 
% test_word in order to test the unifiability of all the words against all the
% slots and creates a list of pairs.
pair_a_word_with_slots([],_,Pairs, Pairs).
pair_a_word_with_slots([Word|Words],Slots, Acc, Pairs):-
        test_word(Word, Slots, UnifiableList),
        pair_a_word_with_slots(Words, Slots, [Word-UnifiableList|Acc], Pairs).

% Tests the unifiability of Word with Slots without
% unifying them! Returns a list of the unifiable slots
test_word(Word, Slots, UnifiableList):-
        test_word(Word, Slots, [], UnifiableList).
test_word(_,[],Slots,Slots).
test_word(Word, [Slot|Slots], Ys, List):-
        (  (unifiable(Word, Slot, _))
        -> test_word(Word, Slots, [Slot|Ys], List)
        ; test_word(Word, Slots, Ys, List)
        ).

% Pack predicate adapted from
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

% -------------- Start Quick Sorts -------------- %
% Adapted from https://www.cp.eng.chula.ac.th/~piak/teaching/dsys/2004/
% quick-prolog.htm

% Used to grab the middle element for pivot selection in
% the quicksort_pair predicate.
middle(List, Middle) :-
    middle(List, List, Middle).
middle([M|_], [_,_], M).
middle([M|_], [_], M).
middle([_|T], [_,_,X|T2], Middle) :-
    middle(T, [X|T2], Middle).

% Starts the quicksort algorithm with the middle element as the pivot
% This is great when we know a list is mostly sorted to avoid choosing the
% worse case pivot.
quicksort_single_mid_pivot(Xs,Ys):-
        middle(Xs, X),
        partition_single(Xs, X, Left, Right),
        quicksort_single(Left, Ls),
        quicksort_single(Right, Rs),
        append(Ls, Rs, Ys).
quicksort_single_mid_pivot([],[]).

% Regular quicksort, chooses first item as pivot.
quicksort_single([X|Xs],Ys) :-
        partition_single(Xs,X,Left,Right),
        quicksort_single(Left,Ls),
        quicksort_single(Right,Rs),
        append(Ls,[X|Rs],Ys).
quicksort_single([],[]).

% Compares the items based on their lengths and partitions them to the Left
% and right accordingly.
partition_single([X|Xs],Y,[X|Ls],Rs) :-
        length(Y, Y_Len),
        length(X, X_Len),
        X_Len =< Y_Len,
        partition_single(Xs,Y,Ls,Rs).
partition_single([X|Xs],Y,Ls,[X|Rs]) :-
        length(Y, Y_Len),
        length(X, X_Len),
        X_Len > Y_Len,
        partition_single(Xs,Y,Ls,Rs).
partition_single([],_,[],[]).

% Same as single mid_pivot quicksort, used to call the pair quicksort
quicksort_pair_mid_pivot(Xs,Ys):-
        middle(Xs, X),
        partition_pair(Xs, X, Left, Right),
        quicksort_pair(Left, Ls),
        quicksort_pair(Right, Rs),
        append(Ls, Rs, Ys).
quicksort_pair_mid_pivot([],[]).

% Same as single quicksort, used to call the partion pairing.
quicksort_pair([X|Xs],Ys) :-
        partition_pair(Xs,X,Left,Right),
        quicksort_pair(Left,Ls),
        quicksort_pair(Right,Rs),
        append(Ls,[X|Rs],Ys).
quicksort_pair([],[]).

% Partitioning key value pairs eg. [Word]-[[Slot]]. Compares the Slots based 
% on their lengths and partitions them to the Left and right accordingly.
partition_pair([Xk-Xv|Xs],Yk-Yv,[Xk-Xv|Ls],Rs) :-
        length(Yv, Y_Len),
        length(Xv, X_Len),
        X_Len =< Y_Len,
        partition_pair(Xs,Yk-Yv,Ls,Rs).
partition_pair([Xk-Xv|Xs],Yk-Yv,Ls,[Xk-Xv|Rs]) :-
        length(Yv, Y_Len),
        length(Xv, X_Len),
        X_Len > Y_Len,
        partition_pair(Xs,Yk-Yv,Ls,Rs).
partition_pair([],_,[],[]).
% -------------- End Quick Sorts -------------- %

% ------------------------------------------------ %
% End Optimisation Functions 
% ------------------------------------------------ %  

% ------------------------------------------------ %
% Start Insertion Functions
% ------------------------------------------------ %

% Inserts words until the crossword is filled. Attempts to unify the first word
% with the first slot.
insert_words([]).
insert_words([Word-[Slot|Slots]|Pairs]):-
        % Attempt to unify, Check validity of unification.
        (  (Word = Slot, update_pairs(Pairs) ) 
        % Successful Unification
        -> true 
        ; % Failed Unification, Try and test against other slots.
           length(Slots, RemainingLen),
           % Fast Failure - No more slots to try.
           RemainingLen > 0,
           insert_words([Word-Slots|Pairs])
        ).
        

% Since we've unified a slot with a word, we have to go back into the pairs
% and test their unifiability with the new slots.
update_pairs(Pairs):-
        update_pairs(Pairs, []).
update_pairs([],Pruned) :-
        % Re-Sort updated list to place word with least slots up the top.
        quicksort_pair_mid_pivot(Pruned, SortedPruned),
        insert_words(SortedPruned).
update_pairs([Word-Slots|Pairs], Acc):-
        % Test unifiability of word against the updated slots.
        test_word(Word, Slots, Remaining),
        length(Remaining, NumRemaining),
        % Fast Failure - 0 matching slots.
        NumRemaining > 0,
        update_pairs(Pairs, [Word-Remaining|Acc]).
% ------------------------------------------------ %
% End Insertion Functions 
% ------------------------------------------------ %  