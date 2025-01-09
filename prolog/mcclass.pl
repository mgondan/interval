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
interval:int_hook(tstat, tstat(_), _, []).
interval:tstat(A, Res, Flags) :-
    interval:interval_(round(A, atomic(2)), Res, Flags).

interval:int_hook(hdrs, hdrs(_), _, []).
interval:hdrs(A, Res, Flags) :-
    interval:interval_(round(A, atomic(1)), Res, Flags).

interval:int_hook(chi2ratio, chi2ratio(_), _, []).
interval:chi2ratio(A, Res, Flags) :-
    interval:interval_(round(A, atomic(2)), Res, Flags).

interval:int_hook(pval, pval(_), _, []).
interval:pval(A, Res, Flags) :-
    interval:interval_(round(A, atomic(3)), Res, Flags).

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

interval:int_hook(=@=, equal1(_, _), _, []).
interval:equal1(A, B, Res, Flags) :-
    interval:interval_(A =:= B, Res, Flags).

%
% Confidence intervals
%
interval:data_type(C, Res, Flags) :-
    C = ci(A, B),
    interval:interval_(A, A1, Flags),
    interval:interval_(B, B1, Flags),
    Res = ci(A1, B1).

interval:instantiate(A, Res),
    A = ci
 => Res = ci(_, _).

% Addition CI
interval:int_hook(+, ciplus1(ci, _), ci, []).
interval:ciplus1(ci(A, B), C, Res, Flags) :-
    interval:interval_(A + C, A1, Flags),
    interval:interval_(B + C, B1, Flags),
    Res = ci(A1, B1).

interval:int_hook(+, ciplus2(_, ci), ci, []).
interval:ciplus2(C, ci(A, B), Res, Flags) :-
    interval:ciplus1(ci(A, B), C, Res, Flags).

% Subtraction CI
interval:int_hook(-, ciminus(ci, _), ci, []).
interval:ciminus(ci(A, B), C, Res, Flags) :-
    interval:interval_(A - C, A1, Flags),
    interval:interval_(B - C, B1, Flags),
    Res = ci(A1, B1).

% Multiplication CI
interval:int_hook(*, cimult(ci, _), ci, []).
interval:cimult(ci(A, B), C, Res, Flags) :-
    interval:interval_(A * C, A1, Flags),
    interval:interval_(B * C, B1, Flags),
    Res = ci(A1, B1).

% Division CI
interval:int_hook(/, cidiv(ci, _), ci, []).
interval:cidiv(ci(A, B), C, Res, Flags) :-
    interval:interval_(A / C, A1, Flags),
    interval:interval_(B / C, B1, Flags),
    Res = ci(A1, B1).

% Exponential CI
interval:int_hook(exp, ciexp(ci), ci, []).
interval:ciexp(ci(A, B), Res, Flags) :-
    interval:interval_(exp(A), A1, Flags),
    interval:interval_(exp(B), B1, Flags),
    Res = ci(A1, B1).