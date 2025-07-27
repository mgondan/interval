/** <file> Evaluation through clpBNR

Used by module 'interval'
*/

:- use_module(library(clpBNR)).

eval(Expr) :-
    {Expr}.

eval(floor(A0), Res) :-
    midpoint(A0, A),
    {Res is floor(A)}.

eval(ceiling(A0), Res) :-
    midpoint(A0, A),
    {Res is ceiling(A)}.

eval(Expr, Res) :-
    {Res is Expr}.

eval(Expr1, Expr2, L...U) :-
    eval(Expr1, L),
    eval(Expr2, U).


