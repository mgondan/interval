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
int_hook(dfrac, frac0(_, _), _, []).
int_hook(frac, frac0(_, _), _, []).
frac0(A, B, Res, Flags) :-
    option(digits(Dig), Flags, _),
    interval_(round(A, atomic(Dig)), A1, Flags),
    interval_(round(B, atomic(Dig)), B1, Flags),
    !,
    interval_(A1 / B1, L...U, Flags),
    return(L, U, Res).
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
pval(A, pval(Res), Flags) :-
    interval_(A, Res, Flags).

%
% Bugs
%
% Forget parts of an expression
int_hook(omit_left, left(_, _), _, [evaluate(false)]).
left(_Bug, A, Res, Flags) :-
    A =.. [_Op, _L, R],
    interval_(R, Res, Flags).

int_hook(omit_right, right(_, _), _, [evaluate(false)]).
right(_Bug, A, Res, Flags) :-
    A =.. [_Op, L, _R],
    interval_(L, Res, Flags).

int_hook(omit, omit(_, _), _, [evaluate(false)]).
omit(_Bug, _Expr, na, _Flags).

% Instead
int_hook(instead, instead1(_, _, _), _, [evaluate(false)]).
instead1(_Bug, Wrong, _Correct, Res, Flags) :-
    interval_(Wrong, Res, Flags).

int_hook(instead, instead2(_, _, _, _), _, [evaluate(false)]).
instead2(_Bug, Wrong, _Correct, _Correct0, Res, Flags) :-
    interval_(Wrong, Res, Flags).

% Drop
int_hook(drop_right, drop_right(_, _), _, [evaluate(false)]).
drop_right(Bug, A, Res, Flags) :-
    right(Bug, A, Res, Flags).

int_hook(drop_left, drop_left(_, _), _, [evaluate(false)]).
drop_left(Bug, A, Res, Flags) :-
    left(Bug, A, Res, Flags).

% add_left, add_right
int_hook(add_right, add(_, _), _, [evaluate(false)]).
add(_Bug, A, Res, Flags) :-
    interval_(A, Res, Flags).

int_hook(add_left, add(_, _), _, [evaluate(false)]).

%
% Multiply
%
int_hook(dot, dot(_, _), _, []).
dot(A, B, Res, Flags) :-
    interval_(A * B, Res, Flags).

%
% Available: not NA
%
int_hook(available, avail0(_), _, []).
avail0(pval(A), Res, Flags) :-
    !,
    interval(available1(A), Res, Flags).

avail0(A, Res, Flags) :-
    interval(available1(A), Res, Flags).

int_hook(available1, avail1(atomic), _, []).
avail1(atomic(A), Res, _Flags) :-
    (  avail2(atomic(A), _Res1)
    -> Res = true 
    ;  Res = false
    ).

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

int_hook(available1, avail3(...), _, []).
avail3(A ... B, Res, _Flags)
 => avail2(atomic(A), A1),
    avail2(atomic(B), B1),
    (  eval(A1, B1, _)
    -> Res = true
    ;  Res = false
    ).

int_hook(available1, avail4(ci), _, []).
avail4(ci(A, B), Res, Flags)
 => ( interval_(available(A), true, Flags),
      interval_(available(B), true, Flags)
    -> Res = true
    ;  Res = false
    ). 

int_hook(=@=, equal0(_, _), _, []).
equal0(A, pval(B), Res, Flags) :-
    !,
    interval_(equal1(A, B), Res, Flags).

equal0(ci(A, B), ci(C, D), Res, Flags) :-
    !,
    interval_(A =@= C, Res0, Flags),
    interval_(B =@= D, Res1, Flags),
    (   Res0 = true, 
        Res1 = true
    ->  Res = true
    ;   Res = false
    ).

equal0(A, B, Res, Flags) :-
    interval_(equal1(A, B), Res, Flags).

int_hook(equal1, equal1(_, _), _, []).
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

% Return a one-tailed confidence interval
int_hook(neginf, neginf0(_), ci, []).
neginf0(A, Res, _Flags) :-
    Res = ci(A, atomic(1.0Inf)).

int_hook(ninfpos, ninfpos0(_), ci, []).
ninfpos0(A, Res, _Flags) :-
    Res = ci(atomic(-1.0Inf), A).

%
% Equation sign: named arguments in R functions (leave name unchanged)
%
/* int_hook(=, equ(_, _), _, []).
equ(Name, A, Res, Flags) :-
    interval_(A, A1, Flags),
    Res = (Name = A1). */

%
% Denote
%
int_hook(denote, den(_, _, _), _, [evaluate(false)]).
den(_Sym, A, _Text, Res, Flags) :-
    interval_(A, A1, Flags),
    Res = A1.

%
% Color
%
int_hook(color, col(_, _), _, [evaluate(false)]).
col(_Col, A, Res, Flags) :-
    interval_(A, A1, Flags),
    Res = A1.

%
% Read intervals from input
%
int_hook(input, input1(atomic), ..., []).
input1(A, Res, Flags) :-
    option(digits(D), Flags, 2),
    Eps is 10^(-D)/2,
    MEps is -Eps,
    interval_(A + MEps...Eps, Res, Flags).

int_hook(input, input2(ci), ci, []).
input2(ci(A, B), Res, Flags) :-
    option(digits(D), Flags, 2),
    Eps is 10^(-D)/2,
    MEps is -Eps,
    interval_(A + MEps...Eps, A1, Flags),
    interval_(B + MEps...Eps, B1, Flags),
    Res = ci(A1, B1).

%
% Other
%
int_hook(';', or(_, _), _, []).
or(A, B, Res, Flags) :-
    interval_(A, _, Flags),
    interval_(B, Res, Flags).

int_hook(';', or(_, _, _), _, []).
or(A, B, C, Res, Flags) :-
    interval_(A, _, Flags),
    interval_(B, _, Flags),
    interval_(C, Res, Flags).


int_hook('{}', curly(_), _, []).
curly(A, Res, Flags) :-
    interval_(A, Res, Flags).

%
% Upper tail, lower tail, both tails
%
% dist/3 just returns the second argument. It is needed for mathematical
% rendering expressions like P_T(X >= x; df=n-1) via mathml.
%
int_hook(dist, dist(atomic, _, atomic), _, []).
dist(_, X, _, Res, Flags) :-
    interval_(X, Res, Flags).

%
% This forwards the tail argument to lower.tail of the R function, e.g. in
% pt(T, DF, lower.tail=TRUE)
%
int_hook(tail, tail(atomic), atomic, []).
tail(A, A, _).
