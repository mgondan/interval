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
interval:int_hook(pbinom, pbinom(..., ..., ..., atomic)).

% lower tail
interval:pbinom(X, N, P, atomic(true), Res) :-
    !,
    interval(pbinom0(X, N, P), Res).

% upper tail
interval:pbinom(X, N, P, atomic(false), Res) :-
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

