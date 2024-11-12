:- module(rint, []).

:- multifile r_hook/1.

:- reexport(interval).
:- reexport(r), r_initialize.

/** <module> Use intervals in R functions.

This module expands the 'interval' module with R functions.
For general information on the use of interval/2 and interval/3, refer to that module. 
 */

% Binomial distribution
% - Cumulated density lower-tail: interval(pbinom(X, N, P, true), Res)
% - Cumulated density upper-tail: interval(pbinom(X, N, P, false), Res)
% - Quantile: interval(qbinom(Alpha, N, P, true), Res)
% - Density: interval(dbinom(X, N, P), Res)
%
% Normal distribution
% - Cumulated density: interval(pnorm(X, Mu, Sigma), Res)
% - Quantile: interval(qnorm(P, Mu, Sigma), Res)
% - Density: interval(dnorm(X, Mu, Sigma), Res)
%
% T distribution
% - Cumulated density lower-tail: interval(pt(X, Df, true), Res)
% - Cumulated density upper-tail: interval(pt(X, Df, false), Res)
% - Quantile: interval(qt(P, Df), Res)
% - Density: interval(dt(X, Df), Res)
%
% Skip R vectors
%
interval:int_hook(:, colon(_, _)).
colon(A, A).

%
% Obtain atoms or functions from R
%
interval:eval_hook(Atom, Res) :-
    atomic(Atom),
    r_hook(Atom),
    !,
    r(Atom, Res).

interval:eval_hook(Expr, Res) :-
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    r_hook(Name/Arity),
    !,
    r(Expr, Res).

r_hook(true).
r_hook(false).

%
% Binomial distribution
%
interval:int_hook(pbinom, pbinom(atomic, atomic, ..., atomic)).

% lower tail
interval:pbinom(atomic(X), atomic(N), P, atomic(true), Res) :-
    !,
    interval(pbinom0(X, N, P), Res).

% upper tail
interval:pbinom(atomic(X), atomic(N), P, atomic(false), Res) :-
    interval(pbinom1(X, N, P), Res).

r_hook(pbinom0/3).
interval:mono(pbinom0/3, [+, -, -]).

r_hook(pbinom1/3).
interval:mono(pbinom1/3, [-, +, +]).

%
% Quantile function - hier weiter
%
interval:int_hook(qbinom, qbinom(..., ..., ..., atomic)).

% lower tail
interval:qbinom(Alpha, N, P, atomic(true), Res) :-
    !,
    interval(qbinom0(Alpha, N, P), Res).

% upper tail
interval:qbinom(Alpha, N, P, atomic(false), Res) :-
    interval(qbinom1(Alpha, N, P), Res).

r_hook(qbinom0/3).
interval:mono(qbinom0/3, [+, +, +]).

r_hook(qbinom1/3).
interval:mono(qbinom1/3, [-, +, +]).

%
% Density
%
interval:int_hook(dbinom, dbinom(..., ..., ...)).

% left to X / N
interval:dbinom(X1...X2, N1...N2, P1...P2, Res) :-
    X2 < N1 * P1,
    !,
    interval(dbinom0(X1...X2, N1...N2, P1...P2), Res).

% right to X / N
interval:dbinom(X1...X2, N1...N2, P1...P2, Res) :-
    X1 > N2 * P2,
    !,
    interval(dbinom1(X1...X2, N1...N2, P1...P2), Res).

% otherwise
interval:dbinom(X1...X2, N1...N2, P1...P2, Res) :-
    r(dbinom2(X1, X2, N1, N2, P1, P2), #(L, U)),
    Res = L...U.

r_hook(dbinom0/3).
interval:mono(dbinom0/3, [+, -, -]).

r_hook(dbinom1/3).
interval:mono(dbinom1/3, [-, +, +]).

%
% Normal distribution
%
r_hook(pnorm0/1).
interval:mono(pnorm0/1, [+]).

interval:int_hook(pnorm, pnorm(..., ..., ...)).
interval:pnorm(X, Mu, Sigma, Res) :-
     interval((X - Mu)/Sigma, Z),
     interval(pnorm0(Z), Res).

%
% Quantile function
%
r_hook(qnorm0/1).
interval:mono(qnorm0/1, [+]).

interval:int_hook(qnorm, qnorm(..., ..., ...)).
interval:qnorm(P, Mu, Sigma, Res) :-
     interval(qnorm0(P), Z),
     interval(Mu + Z * Sigma, Res).

%
% Density
%
r_hook(dnorm1/1).
interval:mono(dnorm1/1, [+]).

r_hook(dnorm2/1).
interval:mono(dnorm2/1, [-]).

interval:int_hook(dnorm, dnorm(..., ..., ...)).
interval:dnorm(X, Mu, Sigma, Res) :-
    interval((X - Mu)/Sigma, Z),
    interval(1/Sigma * dnorm0(Z), Res).

interval:int_hook(dnorm0, dnorm0(...)).
interval:dnorm0(A...B, Res) :-
    B =< 0,
    !,
    interval(dnorm1(A...B), Res).

interval:dnorm0(A...B, Res) :-
    A >= 0,
    !,
    interval(dnorm2(A...B), Res).

% mixed
interval:dnorm0(A...B, Res) :-
    Max is max(abs(A), B),
    interval(dnorm2(0...Max), Res).

%
% t distribution
%
interval:int_hook(pt, pt(..., ..., atomic)).

r_hook(pt0/2).
interval:mono(pt0/2, [+,-]).

r_hook(pt1/2).
interval:mono(pt1/2, [+,+]).

r_hook(pt2/2).
interval:mono(pt2/2, [-,+]).

r_hook(pt3/2).
interval:mono(pt3/2, [-,-]).

% lower tail
interval:pt(L...U, Df, atomic(true), Res) :-
    U =< 0,
    !,
    interval(pt0(L...U, Df), Res).

interval:pt(L...U, Df, atomic(true), Res) :-
    L >= 0,
    !,
    interval(pt1(L...U, Df), Res).

interval:pt(L...U, Df, atomic(true), Res) :-
    Max is max(abs(L), U), 
    interval(pt1(0...Max, Df), Res).

% upper tail
interval:pt(L...U, Df, atomic(false), Res) :-
    U =< 0,
    !, 
    interval(pt2(L...U, Df), Res).

interval:pt(L...U, Df, atomic(false), Res) :-
    L >= 0,
    !, 
    interval(pt3(L...U, Df), Res).

interval:pt(L...U, Df, atomic(false), Res) :-
    Max is max(abs(L), U), 
    interval(pt3(0...Max, Df), Res).

%
% Quantile function
%
r_hook(qt0/2).
interval:mono(qt0/2, [+,-]).

interval:int_hook(qt, qt(..., ...)).
interval:qt(P, Df, Res) :-
     interval(qt0(P, Df), Res).

%
% Density
%
r_hook(dt0/2).
interval:mono(dt0/2, [+,+]).

r_hook(dt1/2).
interval:mono(dt1/2, [-,+]).

interval:int_hook(dt, dt(..., ...)).
interval:dt(L...U, Df, Res) :-
    U =< 0,
    !,
    interval(dt0(L...U, Df), Res).

interval:dt(L...U, Df, Res) :-
    L >= 0,
    !,
    interval(dt1(L...U, Df), Res).

% mixed
interval:dt(L...U, Df, Res) :-
    Max is max(abs(L), U),
    interval(dt1(0...Max, Df), Res). 