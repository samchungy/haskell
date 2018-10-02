% You can use this code to get started with your fillin puzzle solver.
% Make sure you replace this comment with your own documentation.
:- ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
%	solve_puzzle(Puzzle, Wordlist, Solved),
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

% solve_puzzle(Puzzle, WordList, SolvedPuzzle) :-
	% init_variables(Puzzle, VarPuzzle),
	% init_slots(VarPuzzle, Slots).

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
	init_variables(Rows, [Acc1|Prev], NewPuzzle).

% Initialises the rows of the puzzle, uses NewRow & Prev to accumulator 
% results and newRow returns the results.
init_vars_row([], NewRow, NewRow).
init_vars_row([X|Xs], Prev, NewRow) :-
	(  X = '_'
	-> A = _
	;  % else we have a letter or hashtag
	   A = X
	),
	init_vars_row(Xs, [A|Prev], NewRow).

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
	   -> append([AccSlot], Prev, Slots)
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
	   -> init_slots_row(Xs, [AccSlot|Prev], [], Slots)
	   ;  init_slots_row(Xs, Prev, [], Slots)
	   )
	;  % else X is a character or blank
	   init_slots_row(Xs, Prev, [X|AccSlot], Slots)
	).
	
test_init(PuzzleFile, Output) :-
	read_file(PuzzleFile, Puzzle),
	init_variables(Puzzle, VarPuzzle),
	init_slots(VarPuzzle, Output).
	   
% ------------------------------------------------ %  
% End Initialisation Functions
% ------------------------------------------------ %  
	
% ------------------------------------------------ %
% Start Matching Functions 
% ------------------------------------------------ %  
	
test_solve(PuzzleFile, WordFile, Output) :-
        read_file(PuzzleFile, Puzzle),
        read_file(WordFile, WordList),
        init_variables(Puzzle, VarPuzzle),
        init_slots(VarPuzzle, Slots),
        sort_words_slots(WordList, Slots, Output).

sort_words_slots(WordList, SlotList, KeyPairs) :-
        insertionSort(SlotList, SortedSlots),
        insertionSort(WordList, SortedWords),
        pack(SortedSlots, PackedSlots),
        pack(SortedWords, PackedWords),
        zip()
        store_together_in_pair(SortedWords, SortedSlots, SortedPairs).

% Pack function taken from
% http://www.ic.unicamp.br/~meidanis/courses/problemas-prolog/p09.prolog
% Packs repeated elements into sublists.
pack([],[]).
pack([X|Xs], [Z|Zs]) :-
        transfer(X,Xs,Ys,Z),
        pack(Ys,Zs).

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- dif(X,Y).
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).        

% Increments the counter via pairs 
% eg. item_counter(a,[b-1,a-1],[b-1,a-2]). item_counter(a,[],[a-1]).
item_counter(X,[],[X-1]).
item_counter(X,[X-N|Ys],[X-M|Ys]) :-
	M is N + 1.
item_counter(X,[Y-N|Ys],[Y-N|Z]) :-
	dif(X,Y),
	item_counter(X,Ys,Z).


% Insertion Sort Taken From http://www.cs.princeton.edu/courses/archive/
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