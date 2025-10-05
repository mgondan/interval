/** <file> User interface

For the use of interval/2, refer to the module 'interval'
*/

:- use_module(cleaning).

interval(Expr, Res) :-
    interval(Expr, Res, []).

interval(Expr, Res, Flags) :-
    clean(Expr, Expr1),
    default_digits(Dig, Flags),
    interval_(Expr1, Res1, [digits(Dig) | Flags]),
    clean(Res, Res1).
    
default_digits(Dig, Flags) 
 => ( nb_current(digits, Dig1)
    -> true
    ; Dig1 = 2
    ),
    option(digits(Dig), Flags, Dig1). 