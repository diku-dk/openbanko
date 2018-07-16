bankoplade([R1,R2,R3,R4,R5,R6,R7,R8,R9]) :-
    findall(X1, between(1, 9, X1), R1),
    findall(X2, between(10, 19, X2), R2),
    findall(X3, between(20, 29, X3), R3),
    findall(X4, between(30, 39, X4), R4),
    findall(X5, between(40, 49, X5), R5),
    findall(X6, between(50, 59, X6), R6),
    findall(X7, between(60, 69, X7), R7),
    findall(X8, between(70, 79, X8), R8),
    findall(X9, between(80, 90, X9), R9).

minibankoplade([R1,R2,R3]) :-
    findall(X1, between(1, 3, X1), R1),
    findall(X2, between(10, 12, X2), R2),
    findall(X3, between(20, 22, X3), R3).


bankoboard(0, 0, 0, 0, []).
bankoboard(R, N1, N2, N3, [[x,e,e]|NewBoard]) :-
    succ(M1, N1),
    succ(RR, R),
    bankoboard(RR, M1, N2, N3, NewBoard).
bankoboard(R, N1, N2, N3, [[e,x,e]|NewBoard]) :-
    succ(M2, N2),
    succ(RR, R),
    bankoboard(RR, N1, M2, N3, NewBoard).
bankoboard(R, N1, N2, N3, [[e,e,x]|NewBoard]) :-
    succ(M3, N3),
    succ(RR, R),
    bankoboard(RR, N1, N2, M3, NewBoard).
bankoboard(R, N1, N2, N3, [[x,x,e]|NewBoard]) :-
    succ(M1, N1), succ(M2, N2),
    succ(RR, R),
    bankoboard(RR, M1, M2, N3, NewBoard).
bankoboard(R, N1, N2, N3, [[x,e,x]|NewBoard]) :-
    succ(M1, N1), succ(M3, N3),
    succ(RR, R),
    bankoboard(RR, M1, N2, M3, NewBoard).
bankoboard(R, N1, N2, N3, [[e,x,x]|NewBoard]) :-
    succ(M2, N2), succ(M3, N3),
    succ(RR, R),
    bankoboard(RR, N1, M2, M3, NewBoard).
bankoboard(R, N1, N2, N3, [[x,x,x]|NewBoard]) :-
    succ(M1, N1), succ(M2, N2), succ(M3, N3),
    succ(RR, R),
    bankoboard(RR, M1, M2, M3, NewBoard).


fillrow(Row, [x,e,e], [X,e,e]) :- select(X, Row, _).
fillrow(Row, [e,x,e], [e,X,e]) :- select(X, Row, _).
fillrow(Row, [e,e,x], [e,e,X]) :- select(X, Row, _).
fillrow(Row, [x,x,e], [X,Y,e]) :-
    select(X, Row, NewRow),
    select(Y, NewRow, _),
    Y > X.
fillrow(Row, [x,e,x], [X,e,Z]) :-
    select(X, Row, NewRow),
    select(Z, NewRow, _),
    Z > X.
fillrow(Row, [e,x,x], [e,Y,Z]) :-
    select(Y, Row, NewRow),
    select(Z, NewRow, _),
    Z > Y.
fillrow(Row, [x,x,x], [X,Y,Z]) :-
    select(X, Row, NewRow),
    select(Y, NewRow, NewNewRow),
    select(Z, NewNewRow, _),
    Y > X, Z > Y.

fastbanko([], [], []).
fastbanko([Row|Rest], [TRow|TRest], [BRow|BRest]) :-
    fillrow(Row, TRow, BRow),
    fastbanko(Rest, TRest, BRest).

banko(Xs) :- bankoplade(B), bankoboard(9, 5, 5, 5, BB), fastbanko(B, BB, Xs).
