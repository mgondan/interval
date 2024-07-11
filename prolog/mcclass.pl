:- module(mcint, []).

:- reexport(interval).
:- reexport(rint).

%
% Fractions, i.e., numerator, line, and denominator
%
interval:int_hook(frac/2, []).
interval:int_hook(frac(A, B), Res, Opt) :-
    interval(A / B, Res, Opt).

interval:int_hook(dfrac/2, []).
interval:int_hook(dfrac(A, B), Res, Opt) :-
    interval(A / B, Res, Opt).

%
% Reasonable number of digits
%
interval:int_hook(tstat/1, []).
interval:int_hook(tstat(A), Res, Opt) :-
    interval(A, Res, [digits(2) | Opt]).

interval:int_hook(hdrs/1, []).
interval:int_hook(hdrs(A), Res, Opt) :-
    interval(A, Res, [digits(1) | Opt]).

interval:int_hook(chi2ratio/1, []).
interval:int_hook(chi2ratio(A), Res, Opt) :-
    interval(A, Res, [digits(2) | Opt]).

interval:int_hook(pval/1, []).
interval:int_hook(pval(A), Res, Opt) :-
    interval(A, Res, [digits(3) | Opt]).

%
% Forget parts of an expression
%
interval:int_hook(omit_left/1, [evaluate(false)]).
interval:int_hook(omit_left(Expr), Res, Opt) :-
    Expr =.. [_Op, _L, R],
    interval(R, Res, Opt).

interval:int_hook(omit_right/1, [evaluate(false)]).
interval:int_hook(omit_right(Expr), Res, Opt) :-
    Expr =.. [_Op, L, _R],
    interval(L, Res, Opt).

%
% Multiply
%
interval:int_hook(dot/2, []).
interval:int_hook(dot(A, B), Res, Opt) :-
    interval(A * B, Res, Opt).

%
% Available: not NA
%
% todo: boolean function
interval:int_hook(available/1, []).
interval:int_hook(available(A), Res, Opt) :-
    interval(A, A1, Opt),
    available(A1, Res).

available(A, Res),
   integer(A)
=> interval:eval(A, Res).

available(A, Res),
   number(A)
=> float_class(A, Class),
   dif(Class, nan),
   interval:eval(A, Res).

available(A, Res),
   atom(A)
=> interval:eval(A, A1),
   available(A1, Res).

available(A ... B, Res)
=> available(A, A1),
   available(B, B1),
   eval(A1, B1, Res).

available(ci(A, B), Res)
=> available(A, A1),
   available(B, B1),
   eval(A1, B1, Res).

% For convenience
eval(Expr1, Expr2, L ... U) :-
    interval:eval(Expr1, L),
    interval:eval(Expr2, U).