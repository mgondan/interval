interval_(A, Res, _Flags),
    atomic(A)
 => Res = atomic(A).

interval_(atomic(A), Res, Flags),
    r_hook(A),
    memberchk(topic(_), Flags)
 => r_mcclass:r_topic(A, Res).
/* 
interval_(A, Res, Flags),
    compound(A),
    r_hook(A),
    memberchk(topic(_), Flags),
    compound_name_arguments(A, Name, _),
    maplist(unwrap, A, Args2),
    compound_name_arguments(A1, Name, Args2)
 => r_mcclass:r_topic(A1, Res). */

interval_(C, Res, Flags),
    C = ci(A, B)
 => interval_(A, A1, Flags),
    interval_(B, B1, Flags),
    Res = ci(A1, B1).

instantiate(A, Res),
    A = ci
 => Res = ci(_, _).