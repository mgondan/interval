:- use_module(cleaning).

interval(Expr, Res) :-
    interval(Expr, Res, []).

interval(Expr, Res1, Flags) :-
    clean(Expr, Expr1),
    default_digits(Dig, Flags),
    interval_(Expr1, Res0, [digits(Dig) | Flags]),
    unwrap(Res0, Res1).

default_digits(Dig, Flags) 
 => ( nb_current(digits, Dig1)
    -> true
    ; Dig1 = 2
    ),
    option(digits(Dig), Flags, Dig1). 