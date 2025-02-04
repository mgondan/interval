interval_(atomic(A), Res, _Flags),
    r_hook(_R, A)
 => eval(A, Res1),
    clean(Res1, Res).

interval_(C, Res, Flags),
    C = ci(A, B)
 => interval_(A, A1, Flags),
    interval_(B, B1, Flags),
    Res = ci(A1, B1).

instantiate(A, Res),
    A = ci
 => Res = ci(_, _).