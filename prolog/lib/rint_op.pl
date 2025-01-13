:- reexport('../r'), r_initialize.

%
% Skip R vectors
%
int_hook(:, colon(_, _), _, []).
colon(A, A).

%
% Obtain atoms or functions from R
%
eval_hook(Atom, Res) :-
    atomic(Atom),
    r_hook(Atom),
    !,
    r(Atom, Res).

eval_hook(Expr, Res) :-
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
int_hook(pbinom, pbinom(atomic, atomic, ..., atomic), ..., []).

% lower tail
pbinom(atomic(X), atomic(N), P, atomic(true), Res, Flags) :-
    !,
    interval_(pbinom0(atomic(X), atomic(N), P), Res, Flags).

% upper tail
pbinom(atomic(X), atomic(N), P, atomic(false), Res, Flags) :-
    interval_(pbinom1(atomic(X), atomic(N), P), Res, Flags).

r_hook(pbinom0/3).
mono(pbinom0/3, [+, -, -]).

r_hook(pbinom1/3).
mono(pbinom1/3, [-, +, +]).

%
% Quantile function - hier weiter
%
int_hook(qbinom, qbinom(..., ..., ..., atomic), ..., []).

% lower tail
qbinom(Alpha, N, P, atomic(true), Res, Flags) :-
    !,
    interval_(qbinom0(Alpha, N, P), Res, Flags).

% upper tail
qbinom(Alpha, N, P, atomic(false), Res, Flags) :-
    interval_(qbinom1(Alpha, N, P), Res, Flags).

r_hook(qbinom0/3).
mono(qbinom0/3, [+, +, +]).

r_hook(qbinom1/3).
mono(qbinom1/3, [-, +, +]).

%
% Density
%
int_hook(dbinom, dbinom(..., ..., ...), ..., []).

% left to X / N
dbinom(X1...X2, N1...N2, P1...P2, Res, Flags) :-
    X2 < N1 * P1,
    !,
    interval_(dbinom0(X1...X2, N1...N2, P1...P2), Res, Flags).

% right to X / N
dbinom(X1...X2, N1...N2, P1...P2, Res, Flags) :-
    X1 > N2 * P2,
    !,
    interval_(dbinom1(X1...X2, N1...N2, P1...P2), Res, Flags).

% otherwise
dbinom(X1...X2, N1...N2, P1...P2, Res, _Flags) :-
    r(dbinom2(X1, X2, N1, N2, P1, P2), #(L, U)),
    Res = L...U.

r_hook(dbinom0/3).
mono(dbinom0/3, [+, -, -]).

r_hook(dbinom1/3).
mono(dbinom1/3, [-, +, +]).

%
% Normal distribution
%
r_hook(pnorm0/1).
mono(pnorm0/1, [+]).

int_hook(pnorm, pnorm(..., ..., ...), ..., []).
pnorm(X, Mu, Sigma, Res, Flags) :-
     interval_((X - Mu)/Sigma, Z, Flags),
     interval_(pnorm0(Z), Res, Flags).

int_hook(pnorm, pnorm1(...), ..., []).
pnorm1(Z, Res, Flags) :-
     interval_(pnorm0(Z), Res, Flags).

int_hook(pnorm, pnorm2(atomic), atomic, []).
pnorm2(atomic(Z), atomic(Res), Flags) :-
     eval(pnorm0(Z), Res, Flags).

%
% Quantile function
%
r_hook(qnorm0/1).
mono(qnorm0/1, [+]).

int_hook(qnorm, qnorm(..., ..., ...), ..., []).
qnorm(P, Mu, Sigma, Res, Flags) :-
     interval_(qnorm0(P), Z, Flags),
     interval_(Mu + Z * Sigma, Res, Flags).

int_hook(qnorm, qnorm1(...), ..., []).
qnorm1(P, Res, Flags) :-
     interval_(qnorm0(P), Res, Flags).

int_hook(qnorm, qnorm2(atomic), atomic, []).
qnorm2(atomic(P), atomic(Res), Flags) :-
     eval(qnorm0(P), Res, Flags).

%
% Density
%
r_hook(dnorm1/1).
mono(dnorm1/1, [+]).

r_hook(dnorm2/1).
mono(dnorm2/1, [-]).

int_hook(dnorm, dnorm(..., ..., ...), ..., []).
dnorm(X, Mu, Sigma, Res, Flags) :-
    interval_((X - Mu)/Sigma, Z, Flags),
    interval_(atomic(1)/Sigma * dnorm0(Z), Res, Flags).

int_hook(dnorm0, dnorm0(...), ..., []).
dnorm0(A...B, Res, Flags) :-
    B =< 0,
    !,
    interval_(dnorm1(A...B), Res, Flags).

dnorm0(A...B, Res, Flags) :-
    A >= 0,
    !,
    interval_(dnorm2(A...B), Res, Flags).

% mixed
dnorm0(A...B, Res, Flags) :-
    Max is max(abs(A), B),
    interval_(dnorm2(0...Max), Res, Flags).

%
% t distribution
%
int_hook(pt, pt(..., ..., atomic), ..., []).

r_hook(pt0/2).
mono(pt0/2, [+,-]).

r_hook(pt1/2).
mono(pt1/2, [+,+]).

r_hook(pt2/2).
mono(pt2/2, [-,+]).

r_hook(pt3/2).
mono(pt3/2, [-,-]).

% lower tail
pt(L...U, Df, atomic(true), Res, Flags) :-
    U =< 0,
    !,
    interval_(pt0(L...U, Df), Res, Flags).

pt(L...U, Df, atomic(true), Res, Flags) :-
    L >= 0,
    !,
    interval_(pt1(L...U, Df), Res, Flags).

pt(L...U, Df, atomic(true), Res, Flags) :-
    Max is max(abs(L), U), 
    interval_(pt1(0...Max, Df), Res, Flags).

% upper tail
pt(L...U, Df, atomic(false), Res, Flags) :-
    U =< 0,
    !, 
    interval_(pt2(L...U, Df), Res, Flags).

pt(L...U, Df, atomic(false), Res, Flags) :-
    L >= 0,
    !, 
    interval_(pt3(L...U, Df), Res, Flags).

pt(L...U, Df, atomic(false), Res, Flags) :-
    Max is max(abs(L), U), 
    interval_(pt3(0...Max, Df), Res, Flags).

%
% Quantile function
%
r_hook(qt0/2).
mono(qt0/2, [+,-]).

int_hook(qt, qt(..., ...), ..., []).
qt(P, Df, Res, Flags) :-
    interval_(qt0(P, Df), Res, Flags).

%
% Density
%
r_hook(dt0/2).
mono(dt0/2, [+, +]).

r_hook(dt1/2).
mono(dt1/2, [-, +]).

int_hook(dt, dt(..., ...), ..., []).
dt(L...U, Df, Res, Flags) :-
    U =< 0,
    !,
    interval_(dt0(L...U, Df), Res, Flags).

dt(L...U, Df, Res, Flags) :-
    L >= 0,
    !,
    interval_(dt1(L...U, Df), Res, Flags).

% mixed
dt(L...U, Df, Res, Flags) :-
    Max is max(abs(L), U),
    interval_(dt1(0...Max, Df), Res, Flags). 

%
% chisq
%

int_hook(pchisq, pchisq(..., atomic, atomic), ..., []).

r_hook(pchisq0/2).
mono(pchisq0/2, [+,-]).

r_hook(pchisq1/2).
mono(pchisq1/2, [-,+]).

% lower tail
pchisq(L...U, Df, atomic(true), Res, Flags):-
    !,
    interval_(pchisq0(L...U, Df), Res, Flags).

% upper tail
pchisq(L...U, Df, atomic(false), Res, Flags):-
    !,
    interval_(pchisq1(L...U, Df), Res, Flags).

%
% quantile function
%
int_hook(qchisq, qchisq(..., atomic, atomic), ..., []).

r_hook(qchisq0/2).
mono(qchisq0/2, [+,+]).

r_hook(qchisq1/2).
mono(qchisq1/2, [-,+]).

qchisq(L...U, Df, atomic(true), Res, Flags):-
    !,
    interval_(qchisq0(L...U, Df), Res, Flags).

qchisq(L...U, Df, atomic(false), Res, Flags):-
    interval_(qchisq1(L...U, Df), Res, Flags).

%
% density
%
int_hook(dchisq, dchisq(..., atomic), ..., []).

r_hook(dchisq0/2).
mono(dchisq0/2, [-,/]).

r_hook(dchisq1/2).
mono(dchisq1/2, [+,/]).

% for df<=2
dchisq(L...U, atomic(Df), Res, Flags):-
    Df =< 2,
    !,
    interval_(dchisq0(L...U, atomic(Df)), Res, Flags).

% for df>2
dchisq(L...U, atomic(Df), Res, Flags):-
    dchisq_A(L...U, atomic(Df), Res, Flags).

% for x < mode
dchisq_A(L...U, atomic(Df), Res, Flags) :-
    Mode is Df - 2,
    U =< Mode,
    !,
    interval_(dchisq1(L...U, atomic(Df)), Res, Flags).

% for x > mode
dchisq_A(L...U, atomic(Df), Res, Flags) :-
    Mode is Df - 2,
    L >= Mode,
    !,
    interval_(dchisq0(L...U, atomic(Df)), Res, Flags).

% for L < mode, U > mode
dchisq_A(L...U, atomic(Df), Res, Flags) :-
    interval_(dchisq(atomic(L), atomic(Df)), X1..._, Flags),
    interval_(dchisq(atomic(U), atomic(Df)), X3..._, Flags),
    L1 is min(X1, X3),
    Mode is Df - 2,
    interval_(dchisq(atomic(Mode), atomic(Df)), U1..._, Flags),
    Res = L1...U1.
