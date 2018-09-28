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

solve_puzzle(_Puzzle0, _WordList, _Puzzle).

% Counts word slots in crossword row, Passes to an item counter.
% Returns format of [(len of word)-(no. of occurances)]
% eg. slot_count(["#","_","_","#"],[2-1]) :-
slot_count([],_).
slot_count([X|_Xs], Z) :-
        slot_count(X, [], Z, 0).
slot_count([X|Xs], Y, Z, N) :-
        (  X = '_'
        -> M is N+1,
           K = Y 
        ;  (X = '#'; N > 1)
        -> M = 0,
           item_counter(N, Y, K)
        ;  M = 0,
           K = Y
        ),
        slot_count(Xs, K, Z, M).
slot_count([],Y,Z,N):-
        ( N > 1
        -> item_counter(N, Y, Z)
        ; Z = Y
        ).
        
% Increments the counter via pairs 
% eg. item_counter(a,[b-1,a-1],[b-1,a-2]). item_counter(a,[],[a-1]).
item_counter(X,[],[X-1]).
item_counter(X,[X-N|Ys],[X-M|Ys]) :-
        M is N + 1.
item_counter(X,[Y-N|Ys],[Y-N|Z]) :-
        dif(X,Y),
        item_counter(X,Ys,Z).
test([],_).
test([X|Xs],[Y|Ys]) :-
        ( X = '_'
        -> Y is 3
        ; Y is 4
        ),
        test(Xs,Ys).