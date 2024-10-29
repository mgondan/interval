:- module(rint, [pbinom/5]).

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
pbinom(X, N, P, atomic(true), Res) :-
    !,
    interval(pbinom0(X, N, P), Res).

% upper tail
pbinom(X, N, P, atomic(false), Res) :-
    interval(pbinom1(X, N, P), Res).

r_hook(pbinom0/3).
interval:mono(pbinom0/3, [+, -, -]).

r_hook(pbinom1/3).
interval:mono(pbinom1/3, [-, +, +]).

%
% Quantile function - hier weiter
%
interval:int_hook(qbinom/4, []).

% lower tail
interval:int_hook(qbinom(Alpha, N, P, true), Res, Opt) :-
    !,
    interval(qbinom0(Alpha, N, P), Res, Opt).

r_hook(qbinom0/3).
interval:mono(qbinom0/3, [+, +, +]).

% upper tail
interval:int_hook(qbinom(Alpha, N, P, false), Res, Opt) :-
    interval(qbinom1(Alpha, N, P), Res, Opt).

r_hook(qbinom1/3).
interval:mono(qbinom1/3, [-, +, +]).

%
% Density
%
interval:int_hook(dbinom/3, []).

% left to X / N
interval:int_hook(dbinom(X1...X2, N1...N2, P1...P2), Res, Opt) :-
    X2 < N1 * P1,
    !,
    interval(dbinom0(X1...X2, N1...N2, P1...P2), Res, Opt).

r_hook(dbinom0/3).
interval:mono(dbinom0/3, [+, -, -]).

% right to X / N
interval:int_hook(dbinom(X1...X2, N1...N2, P1...P2), Res, Opt) :-
    X1 > N2 * P2,
    !,
    interval(dbinom1(X1...X2, N1...N2, P1...P2), Res, Opt).

r_hook(dbinom1/3).
interval:mono(dbinom1/3, [-, +, +]).

% otherwise
interval:int_hook(dbinom(X1...X2, N1...N2, P1...P2), Res, _) :-
    r(dbinom2(X1, X2, N1, N2, P1, P2), #(L, U)),
    Res = L...U.

%
% Normal distribution
%
r_hook(pnorm0/1).
interval:mono(pnorm0/1, [+]).

interval:int_hook(pnorm/3, []).
interval:int_hook(pnorm(X, Mu, Sigma), Res, Opt) :-
     interval((X - Mu)/Sigma, Z, Opt),
     interval(pnorm0(Z), Res, Opt).

%
% Quantile function
%
r_hook(qnorm0/1).
interval:mono(qnorm0/1, [+]).

interval:int_hook(qnorm/3, []).
interval:int_hook(qnorm(P, Mu, Sigma), Res, Opt) :-
     interval(qnorm0(P), Z, Opt),
     interval(Mu + Z * Sigma, Res, Opt).

%
% Density
%
r_hook(dnorm1/1).
interval:mono(dnorm1/1, [+]).

r_hook(dnorm2/1).
interval:mono(dnorm2/1, [-]).

interval:int_hook(dnorm/3, []).
interval:int_hook(dnorm(X, Mu, Sigma), Res, Opt) :-
    interval((X - Mu)/Sigma, Z, Opt),
    interval(1/Sigma * dnorm0(Z), Res, Opt).

interval:int_hook(dnorm0/1, []).
interval:int_hook(dnorm0(A...B), Res, Opt) :-
    B =< 0,
    !,
    interval(dnorm1(A...B), Res, Opt).

interval:int_hook(dnorm0(A...B), Res, Opt) :-
    A >= 0,
    !,
    interval(dnorm2(A...B), Res, Opt).

% mixed
interval:int_hook(dnorm0(A...B), Res, Opt) :-
    Max is max(abs(A), B),
    interval(dnorm2(0...Max), Res, Opt).

