/** <file> Evaluation

Depends on these externally defined predicates:
 - r/2

Used by module 'rint'
*/

:- dynamic eval/1.
:- dynamic eval/2.
:- dynamic eval/3.

% Logical expression >, <, etc.
eval(hook(Predicate, Expr)) :-
    !,
    call(Predicate, Expr).

eval(Expr) :-
    Expr.

% Custom predicate for evaluation
eval(hook(Predicate, Expr), Res) :-
    !,
    call(Predicate, Expr, Res).

% Evaluate in R
eval(r(Expr), Res) :-
    !,
    r(Expr, Res).

% Standard arithmetic evaluation
eval(Expr, Res) :-
    Res is Expr.

eval(Expr1, Expr2, L...U) :-
    eval(Expr1, L),
    eval(Expr2, U).


