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
interval:int_hook(available/1, []).
interval:int_hook(available(A), Res, Opt) :-
    interval(A, A1, Opt),
    available(A1, _),
    !,
    Res = true;
    Res = false.

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
   interval:eval(A1, B1, Res).

available(ci(A, B), Res)
=> available(A, A1),
   available(B, B1),
   interval:eval(A1, B1, Res).

%
% Confidence intervals
%
interval:int_hook(ci/2, []).
interval:int_hook(ci(L, H), Res, Opt) :-
    ci(L, H, Res, Opt).

ci(L, _H, Res, Opt),
    option(ci(lower), Opt),
    !
 => interval(L, Res, Opt).

ci(_L, H, Res, Opt),
    option(ci(upper), Opt),
    !
=> interval(H, Res, Opt).

ci(L, H, Res, Opt),
    !
 => interval(ci(L, H), LA, [ci(lower) | Opt]),
    interval(ci(L, H), UB, [ci(upper) | Opt]),
    Res = ci(LA, UB).

% Plus/Minus: return a confidence interval
interval:int_hook(pm/2, _).
interval:int_hook(pm(X, Y), Res, Opt) :-
    option(ci(lower), Opt),
    !,
    interval(X - Y, Res, Opt).

interval:int_hook(pm(X, Y), Res, Opt) :-
    option(ci(upper), Opt),
    !,
    interval(X + Y, Res, Opt).

interval:int_hook(pm(X, Y), Res, Opt) :-
    !,
    interval(pm(X, Y), L, [ci(lower) | Opt]),
    interval(pm(X, Y), U, [ci(upper) | Opt]),
    Res = ci(L, U).

% Return a one-tailed confidence interval
interval:int_hook(neginf/1, []).
interval:int_hook(neginf(X), Res, Opt) :-
    neginf(X, Res, Opt).

neginf(X, Res, Opt), 
    option(ci(lower), Opt),
    !
 => interval(X, Res, Opt).

neginf(_X, Res, Opt), 
    option(ci(upper), Opt),
    !
 => interval(1.0Inf, Res, Opt).

neginf(X, Res, Opt) 
 => !,
    interval(neginf(X), L, [ci(lower) | Opt]),
    interval(neginf(X), U, [ci(upper) | Opt]),
    Res = ci(L, U).

interval:int_hook(ninfpos/1, []).
interval:int_hook(ninfpos(X), Res, Opt) :-
    ninfpos(X, Res, Opt).

ninfpos(_X, Res, Opt),
    option(ci(lower), Opt),
    !
 => interval(-1.0Inf, Res, Opt).

ninfpos(X, Res, Opt),
    option(ci(upper), Opt),
    !
 => interval(X, Res, Opt).

ninfpos(X, Res, Opt) 
 => !,
    interval(ninfpos(X), L, [ci(lower) | Opt]),
    interval(ninfpos(X), U, [ci(upper) | Opt]),
    Res = ci(L, U).

%
% Equality = overlapping
%
interval:int_hook((=@=)/2, []).
interval:int_hook(A =@= B, Res, Opt) :-
    select_option(ci(lower), Opt, New),
    !,
    interval(A, LA, Opt),
    interval(B, LB, Opt),
    interval(LA =@= LB, Res, New).

interval:int_hook(A =@= B, Res, Opt) :-
    select_option(ci(upper), Opt, New),
    !,
    interval(A, UA, Opt),
    interval(B, UB, Opt),
    interval(UA =@= UB, Res, New).

interval:int_hook(A =@= B, Res, Opt) :-
    interval(A, A1, Opt),
    interval(B, B1, Opt),
    overlapping(A1, B1, Res).

overlapping(A ... B, C ... D, Res)
 => interval:eval(max(A, C), min(B, D), L ... U),
    (   L =< U
    ->  Res = true
    ;   Res = false
    ).

overlapping(A, B, Res),
    atomic(A),
    atomic(B)
 => (   A =@= B
    ->  Res = true
    ;   Res = false
    ).