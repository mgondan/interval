:- module(mcint, []).

:- reexport(interval).
:- reexport(rint).

%
% Fractions, i.e., numerator, line, and denominator
%
interval:int_hook(frac, frac(..., ..., atomic), []).
interval:frac(A, B, atomic(Dig), Res) :-
    interval:round1(A, atomic(Dig), A1),
    interval:round1(B, atomic(Dig), B1),
    interval:interval_(A1 / B1, Res).
 
interval:int_hook(dfrac, dfrac(_, _), []).
interval:dfrac(A, B, Res) :-
    interval:interval_(A / B, Res).

%
% Reasonable number of digits
%
interval:int_hook(tstat, tstat(...), []).
interval:tstat(A...B, Res) :-
    interval:round1(A...B, atomic(2), Res).

interval:int_hook(hdrs, hdrs(...), []).
interval:hdrs(A...B, Res) :-
    interval:round1(A...B, atomic(1), Res).

interval:int_hook(chi2ratio, chi2ratio(...), []).
interval:chi2ratio(A...B, Res) :-
    interval:round1(A...B, atomic(2), Res).

interval:int_hook(pval, pval(...), []).
interval:pval(A...B, Res) :-
    interval:round1(A...B, atomic(3), Res).

%
% Forget parts of an expression
%
interval:int_hook(omit_left, omit_left(_), [evaluate(false)]).
interval:omit_left(Expr, Res) :-
    Expr =.. [_Op, _L, R],
    interval:interval_(R, Res).

interval:int_hook(omit_right, omit_right(_), [evaluate(false)]).
interval:omit_right(Expr, Res) :-
    Expr =.. [_Op, L, _R],
    interval:interval_(L, Res).

%
% Multiply
%
interval:int_hook(dot, dot(_, _), []).
interval:dot(A, B, Res) :-
    interval:interval_(A * B, Res).

%
% Available: not NA
%
interval:int_hook(available, avail1(atomic), []).
interval:avail1(atomic(A), Res) :-
    avail2(atomic(A), _),
    !,
    Res = true;
    Res = false.

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

interval:int_hook(available, avail3(...), []).
interval:avail3(A ... B, Res)
=> avail2(atomic(A), A1),
   avail2(atomic(B), B1),
   interval:eval(A1, B1, _),
   !,
   Res = true;
   Res = false.

interval:int_hook(available, avail4(ci), []).
interval:avail4(ci(A, B), Res)
=> avail2(atomic(A), A1),
   avail2(atomic(B), B1),
   interval:eval(A1, B1, _),
   !,
   Res = true;
   Res = false.

interval:int_hook(=@=, equal1(..., ...), []).
interval:equal1(A, B, Res) :-
    interval:interval_(A =:= B, Res).


interval:int_hook(juli, juli(_, atomic), []).
interval:juli(A, atomic(Dig), Res) :-
    interval:round1(A, atomic(Dig), Res).