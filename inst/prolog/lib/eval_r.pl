/** <file> Evaluation

Depends on these externally defined predicates:
 - r/2

Used by module 'rint'
*/

% Logical expression >, <, etc.
eval(Expr) :-
    Expr.

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


