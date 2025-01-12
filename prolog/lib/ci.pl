interval_(atomic(A), Res, Flags),
    r_hook(A),
    memberchk(topic(_), Flags)
 => r_mcclass:r_topic(A, Res).

interval_(C, Res, Flags),
    C = ci(A, B)
 => interval_(A, A1, Flags),
    interval_(B, B1, Flags),
    Res = ci(A1, B1).

instantiate(A, Res),
    A = ci
 => Res = ci(_, _).