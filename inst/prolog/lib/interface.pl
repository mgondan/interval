/** <file> User interface
Depends on these externally defined predicates:
 - clean/2

For the use of interval/2, refert to the module 'interval'
*/

interval(Expr, Res) :-
    interval(Expr, Res, []).

interval(Expr, Res, Flags) :-
    copy_term(Expr, Expr0),
    copy_term(Res, Res0),
    clean(Expr0, Expr1),
    clean(Res0, Res1),
    default_digits(Dig, Flags),
    interval_(Expr1, Res1, [digits(Dig) | Flags]),
    clean(Expr, Expr1),
    clean(Res, Res1).
    
default_digits(Dig, Flags) 
 => ( nb_current(digits, Dig1)
    -> true
    ; Dig1 = 2
    ),
    option(digits(Dig), Flags, Dig1). 