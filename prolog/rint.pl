:- module(rint, []).

:- multifile r_hook/1.

:- reexport(interval).
:- reexport(r), r_initialize.

%
% Skip R vectors
%
interval:int_hook((:)/2).
interval:int_hook(A:B, A:B).

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
interval:int_hook(pbinom/4).

% lower tail
interval:int_hook(pbinom(X, N, P, true), Res) :-
    interval(pbinom0(X, N, P), Res).

r_hook(pbinom0/3).
interval:mono(pbinom0/3, [+, -, -]).

% upper tail
interval:int_hook(pbinom(X, N, P, false), Res) :-
    interval(pbinom1(X, N, P), Res).

r_hook(pbinom1/3).
interval:mono(pbinom1/3, [-, +, +]).

%
% Quantile function
%
interval:int_hook(qbinom/4).

% lower tail
interval:int_hook(qbinom(Alpha, N, P, true), Res) :-
    interval(qbinom0(Alpha, N, P), Res).

r_hook(qbinom0/3).
interval:mono(qbinom0/3, [+, +, +]).

% upper tail
interval:int_hook(qbinom(Alpha, N, P, false), Res) :-
    interval(qbinom1(Alpha, N, P), Res).

r_hook(qbinom1/3).
interval:mono(qbinom1/3, [-, +, +]).

%
% Density
%
interval:int_hook(dbinom/3).

% left to X / N
interval:int_hook(dbinom(X1...X2, N1...N2, P1...P2), Res) :-
    X2 < N1 * P1,
    !,
    interval(dbinom0(X1...X2, N1...N2, P1...P2), Res).

r_hook(dbinom0/3).
interval:mono(dbinom0/3, [+, -, -]).

% right to X / N
interval:int_hook(dbinom(X1...X2, N1...N2, P1...P2), Res) :-
    X1 > N2 * P2,
    !,
    interval(dbinom1(X1...X2, N1...N2, P1...P2), Res).

r_hook(dbinom1/3).
interval:mono(dbinom1/3, [-, +, +]).

% otherwise
interval:int_hook(dbinom(X1...X2, N1...N2, P1...P2), Res) :-
    r(dbinom2(X1, X2, N1, N2, P1, P2), #(L, U)),
    Res = L...U.

rint :-
     K = 10 ... 11,
     N = 20 ... 21,
     Pi = 0.50 ... 0.55,
     interval(pbinom(K, N, Pi, true), Res),
     writeln(pbinom(K, N, Pi) --> Res).

rint :-
     K = 10 ... 11,
     N = 20 ... 21,
     Pi = 0.50 ... 0.55,
     interval(pbinom0(K, N, Pi), Res),
     writeln(pbinom0(K, N, Pi) --> Res).

rint :-
     K = 10 ... 11,
     N = 20 ... 21,
     Pi = 0.50 ... 0.55,
     interval(pbinom1(K, N, Pi), Res),
     writeln(pbinom1(K, N, Pi) --> Res).

rint :-
     Alpha = 0.05 ... 0.06,
     N = 60 ... 65,
     Pi = 0.50 ... 0.55,
     interval(qbinom(Alpha, N, Pi, true), Res),
     writeln(qbinom(Alpha, N, Pi) --> Res).

rint :-
     K = 10...11,
     N = 20...21,
     Pi = 0.60 ... 0.65,
     interval(dbinom(K, N, Pi), Res),
     writeln(dbinom(K, N, Pi) --> Res).

rint :-
     K = 14...15,
     N = 20...21,
     Pi = 0.60 ... 0.65,
     interval(dbinom(K, N, Pi), Res),
     writeln(dbinom(K, N, Pi) --> Res).

rint :-
     K = 10...15,
     N = 20...21,
     Pi = 0.60 ... 0.65,
     interval(dbinom(K, N, Pi), Res),
     writeln(dbinom(K, N, Pi) --> Res).

