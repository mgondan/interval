/** <file> Evaluation used by module 'interval'
*/

:- dynamic eval/1.
:- dynamic eval/2.
:- dynamic eval/3.

eval(hook(Predicate, Expr)) :-
    !,
    call(Predicate, Expr).

eval(Expr) :-
    Expr.

eval(hook(Predicate, Expr), Res) :-
    !,
    call(Predicate, Expr, Res).

eval(Expr, Res) :-
    Res is Expr.

eval(Expr1, Expr2, L...U) :-
    eval(Expr1, L),
    eval(Expr2, U).


