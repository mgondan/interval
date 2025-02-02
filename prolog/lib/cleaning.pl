:- module(cleaning, [clean/2, unwrap/2, unwrap_r/2, op(150, xfx, ...)]).

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

unwrap_r(A, Res)
 => unwrap_r_(A, Res).

unwrap_r_(atomic(A), Res)
 => Res = A.

unwrap_r_(A, _Res),
    A = _..._
 => fail.

unwrap_r_(A, Res),
    compound(A)
 => mapargs(unwrap_r_, A, Res).