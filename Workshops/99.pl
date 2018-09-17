% http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
%1
my_last(A,[A]).
my_last(A,[_X|XS]) :-
        my_last(A,XS).
        
%2
snd_last(A,[A,_]).
snd_last(A,[_X|XS]) :-
        snd_last(A,XS).
        
%3
kth_el(X,[X|_Xs],1).
kth_el(A,[_B|Bs],N) :-
        N > 1, 
        kth_el(A,Bs,M),
        M is N - 1.
        
%4
num_el(0,[]).
num_el(N,[_|Xs]) :-
        num_el(K,Xs),
        N is K + 1.
        
%6
rev([],Z,Z).
rev([H|T],Z,Acc) :-
        rev(T,Z,[H|Acc]).
        
rev2(L1, L2) :-
        samelength(L1,L2),
        rev(L1,L2,[]).

samelength([], []).
samelength([_|Xs], [_|Ys]) :-
samelength(Xs, Ys).

%7
my_flatten(X,[X]) :- \+ is_list(X).
my_flatten([],[]).
my_flatten(X|Xs,Ys) :-
        my_flatten(Xs, Zs),
        my_flatten(X, Y),
        append(Y,Zs,Ys).
        