:- module(cleaning, [clean/2, unwrap/2, op(150, xfx, ...)]).

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