% For mcclass. Todo: change later 
interval_(A, Res, _Flags),
    atomic(A)
 => Res = atomic(A).

interval_(atomic(A), Res, Flags),
    r_hook(A),
    memberchk(topic(_), Flags)
 => r_session:r_topic(A, Res1),
    Res = Res1.

interval_(C, Res, Flags),
    C = ci(A, B)
 => interval_(A, A1, Flags),
    interval_(B, B1, Flags),
    Res = ci(A1, B1).

instantiate(A, Res),
    A = ci
 => Res = ci(_, _).