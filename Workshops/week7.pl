list_of(_L,[]).
list_of(A,[A|C]) :-
        list_of(A,C).

all_same(list) :-
        list_of(_,list).
        
