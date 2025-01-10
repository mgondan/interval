%
% Addition (for testing)
%
int_hook(plus, plus1(atomic, atomic), atomic, []).
plus1(atomic(A), atomic(B), atomic(Res), _Flags) :-
    !,
    writeln(+),
    Res is A + B.

int_hook(plus, plus2(..., ...), ..., []).
plus2(A, B, Res, Flags) :-
    !,
    writeln(+),
    interval_(A + B, Res, Flags).

%
% Fractions, i.e., numerator, line, and denominator
%
int_hook(frac, frac(_, _), _, []).
frac(A, B, Res, Flags) :-
    option(digits(Dig), Flags, _),
    interval_(round(A, atomic(Dig)), A1, Flags),
    interval_(round(B, atomic(Dig)), B1, Flags),
    !,
    interval(A1 / B1, Res, Flags).
 
int_hook(dfrac, dfrac(_, _), _, []).
dfrac(A, B, Res, Flags) :-
    interval(frac(A, B), Res, Flags).

%
% Reasonable number of digits
%
int_hook(tstat, tstat(_), _, []).
tstat(A, Res, Flags) :-
    interval_(round(A, atomic(2)), Res, Flags).

int_hook(hdrs, hdrs(_), _, []).
hdrs(A, Res, Flags) :-
    interval_(round(A, atomic(1)), Res, Flags).

int_hook(chi2ratio, chi2ratio(_), _, []).
chi2ratio(A, Res, Flags) :-
    interval_(round(A, atomic(2)), Res, Flags).

int_hook(pval, pval(_), _, []).
pval(A, Res, Flags) :-
    interval_(round(A, atomic(3)), Res, Flags).

%
% Forget parts of an expression
%
int_hook(omit_left, omit_left(_), _, [evaluate(false)]).
omit_left(Expr, Res, Flags) :-
    Expr =.. [_Op, _L, R],
    interval_(R, Res, Flags).

int_hook(omit_right, omit_right(_), _, [evaluate(false)]).
omit_right(Expr, Res, Flags) :-
    Expr =.. [_Op, L, _R],
    interval_(L, Res, Flags).

%
% Multiply
%
int_hook(dot, dot(_, _), _, []).
dot(A, B, Res, Flags) :-
    interval_(A * B, Res, Flags).

%
% Available: not NA
%
int_hook(available, avail1(atomic), _, []).
avail1(atomic(A), Res, _Flags) :-
    avail2(atomic(A), Res),
    !,
    Res = true 
    ;   Res = false.

avail2(atomic(A), Res),
   integer(A)
=> eval(A, Res).

avail2(atomic(A), Res),
   number(A)
=> float_class(A, Class),
   dif(Class, nan),
   eval(A, Res).

avail2(atomic(A), Res)
=> eval(A, A1),
   avail2(A1, Res).

int_hook(available, avail3(...), _, []).
avail3(A ... B, Res, _Flags)
=> avail2(atomic(A), A1),
   avail2(atomic(B), B1),
   eval(A1, B1, _),
   !,
   Res = true;
   Res = false.

int_hook(=@=, equal1(_, _), _, []).
equal1(A, B, Res, Flags) :-
    interval_(A =:= B, Res, Flags).

% Addition CI
int_hook(+, ciplus1(ci, _), ci, []).
ciplus1(ci(A, B), C, Res, Flags) :-
    interval_(A + C, A1, Flags),
    interval_(B + C, B1, Flags),
    Res = ci(A1, B1).

int_hook(+, ciplus2(_, ci), ci, []).
ciplus2(C, ci(A, B), Res, Flags) :-
    ciplus1(ci(A, B), C, Res, Flags).

% Subtraction CI
int_hook(-, ciminus(ci, _), ci, []).
ciminus(ci(A, B), C, Res, Flags) :-
    interval_(A - C, A1, Flags),
    interval_(B - C, B1, Flags),
    Res = ci(A1, B1).

% Multiplication CI
int_hook(*, cimult(ci, _), ci, []).
cimult(ci(A, B), C, Res, Flags) :-
    interval_(A * C, A1, Flags),
    interval_(B * C, B1, Flags),
    Res = ci(A1, B1).

% Division CI
int_hook(/, cidiv(ci, _), ci, []).
cidiv(ci(A, B), C, Res, Flags) :-
    interval_(A / C, A1, Flags),
    interval_(B / C, B1, Flags),
    Res = ci(A1, B1).

% Exponential CI
int_hook(exp, ciexp(ci), ci, []).
ciexp(ci(A, B), Res, Flags) :-
    interval_(exp(A), A1, Flags),
    interval_(exp(B), B1, Flags),
    Res = ci(A1, B1).

% Plus/minus
int_hook(pm, pm(_, _), ci, []).
pm(A, B, Res, Flags) :-
    interval_(A - B, A1, Flags),
    interval_(A + B, B1, Flags),
    Res = ci(A1, B1).
