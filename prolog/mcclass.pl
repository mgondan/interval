:- module(mcint, []).

:- reexport(interval).
:- reexport(rint).

%
% Fractions, i.e., numerator, line, and denominator
%
interval:int_hook(frac/2).
interval:int_hook(frac(A, B), Res, Opt) :-
    interval(A / B, Res, Opt).

interval:int_hook(dfrac/2).
interval:int_hook(dfrac(A, B), Res, Opt) :-
    interval(A / B, Res), Opt.

%
% Reasonable number of digits
%
interval:int_hook(tstat/1).
interval:int_hook(tstat(A), Res, Opt) :-
    interval(A, R, [digits(2) | Opt]).

%
% Forget parts of an expression
%
interval:int_hook(omit_left/1).
interval:int_hook(omit_left(Expr), Res, Opt) :-
    Expr =.. [_Op, _L, R],
    interval(R, Res, Opt).

interval:int_hook(omit_right/1).
interval:int_hook(omit_right(Expr), Res, Opt) :-
    Expr =.. [_Op, L, _R],
    interval(L, Res, Opt).
