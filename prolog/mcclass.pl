:- module(mcint, []).

:- reexport(interval).
:- reexport(rint).

%
% Addition (for testing)
%
interval:int_hook(plus, plus1(atomic, atomic), atomic, []).
interval:plus1(atomic(A), atomic(B), atomic(Res), _Flags) :-
    !,
    writeln(+),
    Res is A + B.

interval:int_hook(plus, plus2(..., ...), ..., []).
interval:plus2(A, B, Res, Flags) :-
    !,
    writeln(+),
    interval:interval_(A + B, Res, Flags).

%
% Fractions, i.e., numerator, line, and denominator
%
interval:int_hook(frac, frac(_, _), _, []).
interval:frac(A, B, Res, Flags) :-
    option(digits(Dig), Flags, _),
    interval:interval_(round(A, atomic(Dig)), A1, Flags),
    interval:interval_(round(B, atomic(Dig)), B1, Flags),
    !,
    interval:interval(A1 / B1, Res, Flags).
 
interval:int_hook(dfrac, dfrac(_, _), _, []).
interval:dfrac(A, B, Res, Flags) :-
    interval(frac(A, B), Res, Flags).

%
% Reasonable number of digits
%
interval:int_hook(tstat, tstat(...), ..., []).
interval:tstat(A...B, Res, Flags) :-
    interval:interval_(round(A...B, atomic(2)), Res, Flags).

interval:int_hook(hdrs, hdrs(...), ..., []).
interval:hdrs(A...B, Res, Flags) :-
    interval:interval_(round(A...B, atomic(1)), Res, Flags).

interval:int_hook(chi2ratio, chi2ratio(...), ..., []).
interval:chi2ratio(A...B, Res, Flags) :-
    interval:interval_(round(A...B, atomic(2)), Res, Flags).

interval:int_hook(pval, pval(...), ..., []).
interval:pval(A...B, Res, Flags) :-
    interval:interval_(round(A...B, atomic(3)), Res, Flags).

%
% Forget parts of an expression
%
interval:int_hook(omit_left, omit_left(_), _, [evaluate(false)]).
interval:omit_left(Expr, Res, Flags) :-
    Expr =.. [_Op, _L, R],
    interval:interval_(R, Res, Flags).

interval:int_hook(omit_right, omit_right(_), _, [evaluate(false)]).
interval:omit_right(Expr, Res, Flags) :-
    Expr =.. [_Op, L, _R],
    interval:interval_(L, Res, Flags).

%
% Multiply
%
interval:int_hook(dot, dot(_, _), _, []).
interval:dot(A, B, Res, Flags) :-
    interval:interval_(A * B, Res, Flags).

%
% Available: not NA
%
interval:int_hook(available, avail1(atomic), _, []).
interval:avail1(atomic(A), Res, _Flags) :-
    avail2(atomic(A), Res),
    !,
    Res = true 
    ;   Res = false.

avail2(atomic(A), Res),
   integer(A)
=> interval:eval(A, Res).

avail2(atomic(A), Res),
   number(A)
=> float_class(A, Class),
   dif(Class, nan),
   interval:eval(A, Res).

avail2(atomic(A), Res)
=> interval:eval(A, A1),
   avail2(A1, Res).

interval:int_hook(available, avail3(...), _, []).
interval:avail3(A ... B, Res, _Flags)
=> avail2(atomic(A), A1),
   avail2(atomic(B), B1),
   interval:eval(A1, B1, _),
   !,
   Res = true;
   Res = false.

interval:int_hook(=@=, equal1(..., ...), _, []).
interval:equal1(A, B, Res, Flags) :-
    interval:interval_(A =:= B, Res, Flags).
