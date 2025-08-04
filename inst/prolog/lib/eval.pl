/** <file> Evaluation through clpBNR

Used by module 'interval'
*/

:- use_module(library(clpBNR)).

:- dynamic eval/1.
:- dynamic eval/2.
:- dynamic eval/3.

eval(hook(Predicate, Expr)) :-
    !,
    call(Predicate, Expr).

eval(Expr) :-
    {Expr}.

eval(hook(Predicate, Expr), Res) :-
    !,
    call(Predicate, Expr, Res).

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


