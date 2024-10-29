:- module(mcint, []).

:- reexport(interval).
:- reexport(rint).

%
% Fractions, i.e., numerator, line, and denominator
%
interval:int_hook(frac, frac(_, _)).
interval:frac(A, B, Res) :-
    interval(A / B, Res).

interval:int_hook(dfrac, dfrac(_, _)).
interval:dfrac(A, B, Res) :-
    interval(A / B, Res).

%
% Reasonable number of digits
%
interval:int_hook(tstat, tstat(...)).
interval:tstat(A...B, Res) :-
    interval:round1(A...B, atomic(2), Res).

interval:int_hook(hdrs, hdrs(...)).
interval:hdrs(A...B, Res) :-
    interval:round1(A...B, atomic(1), Res).

interval:int_hook(chi2ratio, chi2ratio(...)).
interval:chi2ratio(A...B, Res) :-
    interval:round1(A...B, atomic(2), Res).

interval:int_hook(pval, pval(...)).
interval:pval(A...B, Res) :-
    interval:round1(A...B, atomic(3), Res).

%
% Forget parts of an expression
%
/*interval:int_hook(omit_left/1, [evaluate(false)]).
interval:int_hook(omit_left(Expr), Res, Opt) :-
    Expr =.. [_Op, _L, R],
    interval(R, Res, Opt).*/
interval:int_hook(omit_left, omit_left(_)).
interval:omit_left(Expr, Res) :-
    Expr =.. [_Op, _L, R],
    interval(R, Res).

/*interval:int_hook(omit_right/1, [evaluate(false)]).
interval:int_hook(omit_right(Expr), Res, Opt) :-
    Expr =.. [_Op, L, _R],
    interval(L, Res, Opt).*/
interval:int_hook(omit_right, omit_right(_)).
interval:omit_right(Expr, Res) :-
    Expr =.. [_Op, L, _R],
    interval(L, Res).

%
% Multiply
%
interval:int_hook(dot, dot(_, _)).
interval:dot(A, B, Res) :-
    interval(A * B, Res).

%
% Available: not NA
%
interval:int_hook(available, avail1(atomic)).
interval:avail1(atomic(A), Res) :-
    avail2(A, _),
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

interval:int_hook(available, avail3(...)).
interval:avail3(A ... B, Res)
=> avail2(atomic(A), A1),
   avail2(atomic(B), B1),
   interval:eval(A1, B1, _),
   !,
   Res = true;
   Res = false.

interval:int_hook(available, avail4(ci)).
interval:avail4(ci(A, B), Res)
=> avail2(atomic(A), A1),
   avail2(atomic(B), B1),
   interval:eval(A1, B1, _),
   !,
   Res = true;
   Res = false.
