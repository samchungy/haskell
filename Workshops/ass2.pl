% Sam Chung - 758 053
% Declarative Programming Ass 2

replace(E1, [E1|L1s], E2, [E2|L1s]).
replace(E1, [L2|L1s], E2, [L2|L2s]) :-
        replace(E1, L1s, E2, L2s).

zip([],[],[]).
zip([X|Xs], [Y|Ys], [X-Y|Zs]) :-
        zip(Xs, Ys, Zs).

sublist([],_).
sublist([X|Xs],[X|Ys]) :-
        sublist(Xs,Ys).
sublist([X|Xs],[Y|Ys]) :-
        dif(X,Y),
        sublist([X|Xs],Ys).

test([],Acc,Acc).        
test([X|Xs], Acc, Output) :-
        append(Acc,[X],Acc2),
        test(Xs, Acc2, Output).