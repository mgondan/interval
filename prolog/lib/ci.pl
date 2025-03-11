interval_(atomic(A), Res, _Flags),
    r_hook(_R, A)
 => eval(A, Res1),
    clean(Res1, Res).

interval_(A, Res, _Flags),
    A = 1.0Inf
 => Res = A.

interval_(A, Res, _Flags),
    A = -1.0Inf
 => Res = A.

interval_(C, Res, Flags),
    C = ci(A, B)
 => interval_(A, A1, Flags),
    interval_(B, B1, Flags),
    Res = ci(A1, B1).

instantiate(A, B),
    A = ci
 => B = ci(_, _).

instantiate(A, B),
    B = ci(_, _)
 => A = ci.