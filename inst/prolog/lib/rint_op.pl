:- use_module(cleaning).

%
% Obtain atoms or functions from R
%
eval_hook(r(Expr), Res) :-
    eval_hook(Expr, Res).

eval_hook(r(Expr), Res) :-
    !,
    r(Expr, Res).

eval_hook(Atom, Res) :-
    atomic(Atom),
    r_hook(R, Atom),
    !,
    call(R, Atom, Res).

eval_hook(Atom, Res) :-
    atomic(Atom),
    r_hook(Atom),
    !,
    r(Atom, Res).

eval_hook(Expr, Res) :-
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    r_hook(R, Name/Arity),
    !,
    call(R, Expr, Res).

eval_hook(Expr, Res) :-
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    r_hook(Name/Arity),
    !,
    r(Expr, Res).

%
% Call R 
%
int_hook(r, r1(atomic), _, [evaluate(false)]).
r1(atomic(A), Res, _Flags) :-
    eval(r(A), Res1),
    !,
    clean(Res1, Res).

int_hook(r, r2(_), _, [evaluate(false)]).
r2(A, Res, Flags) :-
    compound(A),
    compound_name_arguments(A, Name, Args1),
    maplist(interval__(Flags), Args1, Args2),
    compound_name_arguments(A1, Name, Args2),
    unwrap_r(A1, A2),
    !,
    eval(r(A2), Res1),
    clean(Res1, Res).

r2(A, Res, Flags) :-
    interval_(A, Res, Flags).

%
% Skip R vectors
%
int_hook(:, colon(_, _), _, []).
colon(A, B, Res, _Flags) :-
    Res =.. [:, A, B].

%
% Binomial distribution
%
int_hook(pbinom, pbinom_(atomic, atomic, atomic), atomic, []).
pbinom_(atomic(X), atomic(N), atomic(P),atomic(Res), _Flags) :-
    eval(r(pbinom(X, N, P)), Res).

int_hook(pbinom, pbinom_(atomic, atomic, atomic, atomic), atomic, []).
pbinom_(atomic(X), atomic(N), atomic(P), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(pbinom(X, N, P, Tail)), Res).

int_hook(pbinom, pbinom(atomic, atomic, ...), ..., []).
pbinom(X, N, P, Res, Flags) :-
    pbinom(X, N, P, atomic(true), Res, Flags).

int_hook(pbinom, pbinom(atomic, atomic, ..., atomic), ..., []).

% lower tail
pbinom(X, N, P, atomic(true), Res, Flags) :-
    !,
    interval_(pbinom0(X, N, P), Res, Flags).

% upper tail
pbinom(X, N, P, atomic(false), Res, Flags) :-
    interval_(pbinom1(X, N, P), Res, Flags).

r_hook(pbinom0/3).
mono(pbinom0/3, [+, -, -]).

r_hook(pbinom1/3).
mono(pbinom1/3, [-, +, +]).

%
% Quantile function
%
int_hook(qbinom, qbinom_(atomic, atomic, atomic), atomic, []).
qbinom_(atomic(Alpha), atomic(N), atomic(P), atomic(Res), _Flags) :-
    eval(r(qbinom(Alpha, N, P)), Res).

int_hook(qbinom, qbinom_(atomic, atomic, atomic, atomic), atomic, []).
qbinom_(atomic(Alpha), atomic(N), atomic(P), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(qbinom(Alpha, N, P, Tail)), Res).

int_hook(qbinom, qbinom2(..., _, ...), ..., []).
qbinom2(Alpha, N, P, Res, Flags) :-
    qbinom(Alpha, N, P, atomic(true), Res, Flags).

int_hook(qbinom, qbinom(..., _, ..., atomic), ..., []).

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
int_hook(dbinom, dbinom_(atomic, atomic, atomic), atomic, []).
dbinom_(atomic(X), atomic(N), atomic(P), atomic(Res), _Flags) :-
    eval(r(dbinom(X, N, P)), Res).

int_hook(dbinom, dbinom3(..., atomic, ...), ..., []).
dbinom3(X1...X2, atomic(N), P1...P2, Res, Flags) :-
    dbinom(X1...X2, N...N, P1...P2, Res, Flags).

int_hook(dbinom, dbinom4(..., atomic, atomic), ..., []).
dbinom4(X1...X2, atomic(N), atomic(P), Res, Flags) :-
    dbinom(X1...X2, N...N, P...P, Res, Flags).

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
    eval(r(dbinom2(X1, X2, N1, N2, P1, P2)), ##(L, U)),
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

r_hook(pnorm1/1).
mono(pnorm1/1, [-]).

% Atomic, Mu = 0, Sd = 1, lower tail
int_hook(pnorm, pnorm_(atomic), atomic, []).
pnorm_(atomic(A), atomic(Res), _Flags) :-
    eval(r(pnorm(A)), Res).

% Atomic, lower tail
int_hook(pnorm, pnorm_(atomic, atomic, atomic), atomic, []).
pnorm_(atomic(A), atomic(Mu), atomic(Sigma), atomic(Res),_Flags) :-
    eval(r(pnorm(A, Mu, Sigma)), Res).

% Atomic
int_hook(pnorm, pnorm_(atomic, atomic, atomic, atomic), atomic, []).
pnorm_(atomic(A), atomic(Mu), atomic(Sigma), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(pnorm(A, Mu, Sigma, Tail)), Res).

% Interval, Mu = 0, Sd = 1, lower tail
int_hook(pnorm, pnorm2(...), ..., []).
pnorm2(A, Res, Flags) :-
    pnorm6(A, atomic(true), Res, Flags).

% Interval, lower tail
int_hook(pnorm, pnorm4(..., ..., ...), ..., []).
pnorm4(A, Mu, Sigma, Res, Flags) :-
    pnorm5(A, Mu, Sigma, atomic(true), Res, Flags).

% Interval
int_hook(pnorm, pnorm5(..., ..., ..., atomic), ..., []).
pnorm5(A, Mu, Sigma, Tail, Res, Flags) :-
     interval_((A - Mu)/Sigma, Z, Flags),
     pnorm6(Z, Tail, Res, Flags).

pnorm6(Z, atomic(true), Res, Flags) :-
     interval_(pnorm0(Z), Res, Flags).

pnorm6(Z, atomic(false), Res, Flags) :-
     interval_(pnorm1(Z), Res, Flags).    

%
% Quantile function
%
r_hook(qnorm0/1).
mono(qnorm0/1, [+]).

r_hook(qnorm1/1).
mono(qnorm1/1, [-]).

% Atomic, Mu = 0, Sd = 1, lower tail
int_hook(qnorm, qnorm_(atomic), atomic, []).
qnorm_(atomic(P), atomic(Res), _Flags) :-
    eval(r(qnorm(P)), Res).

% Atomic, lower tail
int_hook(qnorm, qnorm_(atomic, atomic, atomic), atomic, []).
qnorm_(atomic(P), atomic(Mu), atomic(Sigma), atomic(Res), _Flags) :-
    eval(r(qnorm(P, Mu, Sigma)), Res).

% Atomic
int_hook(qnorm, qnorm_(atomic, atomic, atomic, atomic), atomic, []).
qnorm_(atomic(P), atomic(Mu), atomic(Sigma), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(qnorm(P, Mu, Sigma, Tail)), Res).

% Interval, Mu = 0, Sd = 1, lower tail
int_hook(qnorm, qnorm2(...), ..., []).
qnorm2(P, Res, Flags) :-
     interval_(qnorm0(P), Res, Flags).

% Interval, lower tail  
int_hook(qnorm, qnorm3(..., ..., ...), ..., []).
qnorm3(P, Mu, Sigma, Res, Flags) :-
    qnorm5(P, Mu, Sigma, atomic(true), Res, Flags).

% Interval
int_hook(qnorm, qnorm4(..., ..., ..., atomic), ..., []).
qnorm4(P, Mu, Sigma, Tail, Res, Flags) :-
    qnorm5(P, Mu, Sigma, Tail, Res, Flags).

qnorm5(P, Mu, Sigma, atomic(true), Res, Flags) :-
    interval_(qnorm0(P), Z, Flags),
    interval_(Mu + Z * Sigma, Res, Flags).

qnorm5(P, Mu, Sigma, atomic(false), Res, Flags) :-
    interval_(qnorm1(P), Z, Flags),
    interval_(Mu + Z * Sigma, Res, Flags).

%
% Density
%
r_hook(dnorm0/1).
mono(dnorm0/1, [+]).

r_hook(dnorm1/1).
mono(dnorm1/1, [-]).

% Atomic, Mu = 0, Sd = 1
int_hook(dnorm, dnorm_(atomic), atomic, []).
dnorm_(atomic(A), atomic(Res), _Flags) :-
    eval(r(dnorm(A)), Res).

% Atomic
int_hook(dnorm, dnorm_(atomic, atomic, atomic), atomic, []).
dnorm_(atomic(A), atomic(Mu), atomic(Sigma), atomic(Res), _Flags) :-
    eval(r(dnorm(A, Mu, Sigma)), Res).

% Interval, Mu = 0, Sd = 1 
int_hook(dnorm, dnorm2(...), ..., []).

% Interval 
int_hook(dnorm, dnorm3(..., ..., ...), ..., []).
dnorm3(X, Mu, Sigma, Res, Flags) :-
    interval_((X - Mu)/Sigma, Z, Flags),
    dnorm2(Z, Res0, Flags),
    interval_(atomic(1)/Sigma * Res0, Res, Flags).

dnorm2(A...B, Res, Flags) :-
    B =< 0,
    !,
    interval_(dnorm0(A...B), Res, Flags).

dnorm2(A...B, Res, Flags) :-
    A >= 0,
    !,
    interval_(dnorm1(A...B), Res, Flags).

% mixed
dnorm2(A...B, Res, Flags) :-
    Max is max(abs(A), B),
    interval_(dnorm1(0...Max), Res, Flags).

%
% t distribution
%
r_hook(pt0/2).
mono(pt0/2, [+,+]).

r_hook(pt1/2).
mono(pt1/2, [-,-]).

% Atomic, lower tail
int_hook(pt, pt_(atomic, atomic), atomic, []).
pt_(atomic(A), atomic(Df), atomic(Res), _Flags) :-
    eval(r(pt(A, Df)), Res).

% Atomic
int_hook(pt, pt_(atomic, atomic, atomic), atomic, []).

pt_(atomic(A), atomic(Df), atomic("lower"), atomic(Res), _Flags) :-
    eval(r(pt(A, Df, 'lower.tail'=true)), Res).
pt_(atomic(A), atomic(Df), atomic("upper"), atomic(Res), _Flags) :-
    eval(r(pt(A, Df, 'lower.tail'=false)), Res).
pt_(atomic(A), atomic(Df), atomic("two.sided"), atomic(Res), _Flags) :-
    eval(2 * r(pt(abs(A), Df, 'lower.tail'=false)), Res).
pt_(atomic(A), atomic(Df), atomic("density"), atomic(Res), _Flags) :-
    eval(r(dt(A, Df)), Res).
    
pt_(atomic(A), atomic(Df), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(pt(A, Df, 'lower.tail'=Tail)), Res).

% Interval, lower tail
int_hook(pt, pt2(..., _), ..., []).
pt2(A, Df, Res, Flags) :-
    pt(A, Df, atomic(true), Res, Flags).

% Interval
int_hook(pt, pt(..., _, atomic), ..., []).

pt(A, Df, atomic("lower"), Res, Flags) :-
    pt(A, Df, atomic(true), Res, Flags).
pt(A, Df, atomic("upper"), Res, Flags) :-
    pt(A, Df, atomic(false), Res, Flags).
pt(A, Df, atomic("two.sided"), Res, Flags) :-
    interval_(atomic(2) * pt(abs(A), Df, atomic("upper")), Res, Flags).
pt(A, Df, atomic("density"), Res, Flags) :-
    interval_(dt(A, Df), Res, Flags).

% lower tail
pt(L...U, Df, atomic(true), Res, Flags) :-
    !,
    interval_(pt0(L...U, Df), Res, Flags).

% upper tail
pt(L...U, Df, atomic(false), Res, Flags) :-
    !, 
    interval_(pt1(L...U, Df), Res, Flags).

%
% Quantile function
%
r_hook(qt0/2).
mono(qt0/2, [+,-]).

r_hook(qt1/2).
mono(qt1/2, [-,+]).

% Atomic, lower tail
int_hook(qt, qt_(atomic, atomic), atomic, []).
qt_(atomic(P), atomic(Df), atomic(Res), _Flags) :-
    eval(r(qt(P, Df)), Res).

% Atomic
int_hook(qt, qt_(atomic, atomic, atomic), atomic, []).
qt_(atomic(P), atomic(Df), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(qt(P, Df, 'lower.tail'=Tail)), Res).

% Interval, lower tail
int_hook(qt, qt2(..., _), ..., []).
qt2(P, Df, Res, Flags) :-
    qt(P, Df, atomic(true), Res, Flags).

% Interval
int_hook(qt, qt(..., _, atomic), ..., []).

% lower tail
qt(P, Df, atomic(true), Res, Flags) :-
    interval_(qt0(P, Df), Res, Flags).

% upper tail
qt(P, Df, atomic(false), Res, Flags) :-
    interval_(qt1(P, Df), Res, Flags).

%
% Density
%
r_hook(dt0/2).
mono(dt0/2, [+, +]).

r_hook(dt1/2).
mono(dt1/2, [-, +]).

int_hook(dt, dt_(atomic, atomic), atomic, []).
dt_(atomic(A), atomic(Df), atomic(Res), _Flags) :-
    eval(r(dt(A, Df)), Res).

int_hook(dt, dt(..., _), ..., []).
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
r_hook(pchisq0/2).
mono(pchisq0/2, [+,-]).

r_hook(pchisq1/2).
mono(pchisq1/2, [-,+]).

% Atomic, lower tail
int_hook(pchisq, pchisq_(atomic, atomic), atomic, []).
pchisq_(atomic(A), atomic(Df), atomic(Res), _Flags) :-
    eval(r(pchisq(A, Df)), Res).

% Atomic
int_hook(pchisq, pchisq_(atomic, atomic, atomic), atomic, []).
pchisq_(atomic(A), atomic(Df), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(pchisq(A, Df, 'lower.tail'=Tail)), Res).

% Interval, lower tail
int_hook(pchisq, pchisq2(..., atomic), ..., []).
pchisq2(A, Df, Res, Flags) :-
    pchisq(A, Df, atomic(true), Res, Flags).

% Interval
int_hook(pchisq, pchisq(..., atomic, atomic), ..., []).

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
r_hook(qchisq0/2).
mono(qchisq0/2, [+,+]).

r_hook(qchisq1/2).
mono(qchisq1/2, [-,+]).

% Atomic, lower tail
int_hook(qchisq, qchisq_(atomic, atomic), atomic, []).
qchisq_(atomic(P), atomic(Df), atomic(Res), _Flags) :-
    eval(r(qchisq(P, Df)), Res).

% Atomic
int_hook(qchisq, qchisq_(atomic, atomic, atomic), atomic, []).
qchisq_(atomic(P), atomic(Df), atomic(Tail), atomic(Res), _Flags) :-
    eval(r(qchisq(P, Df, 'lower.tail'=Tail)), Res).

% Interval, lower tail
int_hook(qchisq, qchisq2(..., atomic), ..., []).
qchisq2(P, Df, Res, Flags) :-
    qchisq(P, Df, atomic(true), Res, Flags).

% Interval
int_hook(qchisq, qchisq(..., atomic, atomic), ..., []).

% lower tail
qchisq(L...U, Df, atomic(true), Res, Flags):-
    !,
    interval_(qchisq0(L...U, Df), Res, Flags).

% upper tail
qchisq(L...U, Df, atomic(false), Res, Flags):-
    interval_(qchisq1(L...U, Df), Res, Flags).

%
% density
%
r_hook(dchisq0/2).
mono(dchisq0/2, [-,/]).

r_hook(dchisq1/2).
mono(dchisq1/2, [+,/]).

int_hook(dchisq, dchisq_(atomic, atomic), atomic, []).
dchisq_(atomic(A), atomic(Df), atomic(Res), _Flags) :-
    eval(r(dchisq(A, Df)), Res).

int_hook(dchisq, dchisq(..., atomic), ..., []).
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
    interval_(dchisq(atomic(L), atomic(Df)), atomic(X1), Flags),
    interval_(dchisq(atomic(U), atomic(Df)), atomic(X2), Flags),
    L1 is min(X1, X2),
    Mode is Df - 2,
    interval_(dchisq(atomic(Mode), atomic(Df)), atomic(U1), Flags),
    Res = L1...U1.

%
% Assignment
%
r_hook('<-'/2).
int_hook('<-', assign0(_, _), _, [evaluate(false)]).
assign0(Var, A, Res, Flags) :-
    interval_(A, A1, Flags),
    interval2_(assign(Var, A1), Res, Flags).

int_hook(assign, assign1(atomic, atomic), atomic, []).
assign1(atomic(Var), atomic(A), Res, _Flags) :-
    eval(Var <- A, Res1),
    clean(Res1, Res).

int_hook(assign, assign2(atomic, ...), ..., []).
assign2(atomic(Var), L...U, Res, _Flags) :-
    eval(Var <- call("...", L, U), Res1),
    clean(Res1, Res).
