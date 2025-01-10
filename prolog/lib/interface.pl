interval(Expr, Res) :-
    interval(Expr, Res, []).

interval(Expr, Res1, Flags) :-
    clean(Expr, Expr1),
    default_digits(Dig, Flags),
    interval_(Expr1, Res0, [digits(Dig) | Flags]),
    unwrap(Res0, Res1).

default_digits(Dig, Flags) 
 => nb_getval(digits, Dig1),
    option(digits(Dig), Flags, Dig1).

clean(atomic(A), Res)
 => Res = atomic(A).

clean(L...U, Res)
 => Res = L...U.

clean(Expr, Expr1),
    compound(Expr)
 => mapargs(clean, Expr, Expr1).

clean(A, Res),
    atomic(A)
 => Res = atomic(A).

unwrap(atomic(A), Res)
 => Res = A.

unwrap(A...A, Res)
 => Res = A.

unwrap(A, Res),
    compound(A)
 => mapargs(unwrap, A, Res).

unwrap(A, Res)
 => Res = A.
