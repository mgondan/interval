/** <file> R functions

Depends on these externally defined predicates:
- eval/1
- eval/2
- positive/2
- negative/2

Every operator is defined with an interval_/3 clause. 
Macros may be used for automatic generation of interval_/3 clauses. 
A 'macro' predicate matches the arguments of 'macro_clause' except for the last one being the result:
    macro(Op/Arity, MacroType) <---> macro_clause(Op/Arity, MacroType, Clauses)
    macro(Op/Arity, MacroType, ExtraArg) <---> macro_clause(Op/Arity, MacroType, ExtraArg, Clauses)
    macro(Op/Arity, Fn/Arity, Dir) <---> macro_clause(Op/Arity, Fn/Arity, Dir, Clauses)
    macro(Op/Arity, Fn/Arity, ExtraArg, Dir) <---> macro_clause(Op/Arity, Fn/Arity, ExtraArg, Dir, Clauses)

For more information on the meaning of macro arguments, refer to the module 'expansion'.
*/

:- use_module(expansion).
:- use_module(cleaning).

:- multifile(interval_/3).

:- dynamic(interval_/3).

user:term_expansion(macro(Op/Arity, Fn, Dir), Clauses) :-
    macro_clause(Op/Arity, Fn, Dir, Clauses).

user:term_expansion(macro(Op/Arity, Fn, Dir, Options), Clauses) :-
    macro_clause(Op/Arity, Fn, Dir, Options, Clauses).

%
% Call R 
%
interval_(r(atomic(A)), Res, _Flags) :-
    eval(r(A), Res0),
    !, clean(Res0, Res).

interval_(r(number(A)), Res, _Flags) :-
    eval(r(A), Res0),
    !, clean(Res0, Res).

interval_(r(bool(A)), Res, _Flags) :-
    eval(r(A), Res0),
    !, clean(Res0, Res).

interval_(r(string(A)), Res, _Flags) :-
    eval(r(A), Res0),
    !, clean(Res0, Res).

interval_(r(A), Res, Flags) :-
    compound(A),
    compound_name_arguments(A, Name, Args1),
    maplist(interval__(Flags), Args1, Args2),
    \+ memberchk(_..._, Args2),
    compound_name_arguments(A1, Name, Args2),
    clean(A2, A1),
    eval(r(A2), Res0),
    !, clean(Res0, Res).

interval_(r(A), Res, Flags) :-
    !, interval_(A, Res, Flags).

%
% Assignment in R
%
interval_(atomic(Var) <- number(A), Res, _Flags) :-
    eval(r(Var <- A), Res0),
    !, Res = number(Res0).

interval_(atomic(Var) <- L...U, Res, _Flags) :-
    eval(r(Var <- call("...", L, U)), Res0),
    !, Res = Res0.

interval_(atomic(Var) <- Expr, Res, Flags) :-
    interval_(Expr, Res0, Flags),
    !,
    interval_(atomic(Var) <- Res0, Res, Flags).

%
% Skip R vectors
%
interval_(:(A, B), Res, _Flags) :-
    !, Res = :(A, B).

%
% Binomial distribution
%
% pbinom/3: default lower.tail = TRUE
macro(pbinom/3, all, [+, -, -], [hook(r)]).

% pbinom/4: explicit tail argument
macro(pbinom/4, all, [+, -, -, /], [hook(r), pattern([_, _, _, bool(true)])]).

macro(pbinom/4, all, [-, +, +, /], [hook(r), pattern([_, _, _, bool(false)])]).

% pbinom/5: explicit tail and log.p arguments
macro(pbinom/5, all, [+, -, -, /, /], [hook(r), pattern([_, _, _, bool(true), bool(_)])]).

macro(pbinom/5, all, [-, +, +, /, /], [hook(r), pattern([_, _, _, bool(false), bool(_)])]).

% qbinom/3: default lower.tail = TRUE
macro(qbinom/3, all, [+, +, +], [hook(r)]).

% qbinom/4: explicit tail argument
macro(qbinom/4, all, [+, +, +, /], [hook(r), pattern([_, _, _, bool(true)])]).

macro(qbinom/4, all, [-, +, +, /], [hook(r), pattern([_, _, _, bool(false)])]).

% qbinom/5: explicit tail and log.p arguments
macro(qbinom/5, all, [+, +, +, /, /], [hook(r), pattern([_, _, _, bool(true), bool(_)])]).

macro(qbinom/5, all, [-, +, +, /, /], [hook(r), pattern([_, _, _, bool(false), bool(_)])]).

% dbinom/3
interval_(dbinom(number(Alpha), number(N), number(P)), Res, _Flags) :-
    dbinom_(Alpha, N, P, false, Res0),
    !, Res = number(Res0).

dbinom_(Alpha, N, P, Log, Res) :-
    eval(r(dbinom(Alpha, N, P, 'log'=Log)), Res).

interval_(dbinom(X1...X2, N1...N2, P1...P2), Res, _Flags) :-
    dbinom0(X1...X2, N1...N2, P1...P2, false, Res0),
    !, Res = Res0.

% left to X / N: [+, -, -]
dbinom0(X1...X2, N1...N2, P1...P2, Log, L...U) :-
    eval(X2 < N1 * P1),
    !,
    dbinom_(X1, N2, P2, Log, L),
    dbinom_(X2, N1, P1, Log, U).

% right to X / N: [-, +, +]
dbinom0(X1...X2, N1...N2, P1...P2, Log, L...U) :-
    eval(X1 > N2 * P2),
    !,
    dbinom_(X2, N1, P1, Log, L),
    dbinom_(X1, N2, P2, Log, U).

% otherwise
dbinom0(K1...K2, N1...N2, P1...P2, Log, L...U) :-
    eval(r(g <- 'expand.grid'('k'=K1:K2, 'N'=N1:N2)), _),
    eval(r(g <- subset(g, '&'('$'(g, k) >= floor(P1 * '$'(g, 'N')), '<='('$'(g, k), ceiling(P2 * '$'(g, 'N')))))), _),
    eval(r(c(min(dbinom(c(K1, K2), c(N2, N1), c(P2, P1), 'log'=Log)), 
            max(dbinom('$'(g, k), '$'(g, 'N'), ifelse('$'(g, k) > '$'(g, 'N') * P2, P2, 
                ifelse('$'(g, k) < '$'(g, 'N') * P1, P1, '$'(g, k)/'$'(g, 'N'))), 'log'=Log)))), ##(L, U)).

macro(dbinom/3, interval_, []).

% dbinom/4
interval_(dbinom(number(Alpha), number(N), number(P), bool(Log)), Res, _Flags) :-
    dbinom_(Alpha, N, P, Log, Res0),
    !, Res = number(Res0).

interval_(dbinom(X1...X2, N1...N2, P1...P2, bool(Log)), Res, _Flags) :-
    dbinom0(X1...X2, N1...N2, P1...P2, Log, Res0),
    !, Res = Res0.

macro(dbinom/4, interval_, [], [pattern([_, _, _, bool(_)])]).

%
% Normal distribution
%
% pnorm/1: Mu = 0, Sd = 1, lower.tail = TRUE
macro(pnorm/1, all, [+], [hook(r)]).

% pnorm/3: lower.tail = TRUE
macro(pnorm/3, all, [+,-,+], [hook(r)]).

% pnorm/4: explicit tail argument
macro(pnorm/4, all, [+,-,+,/], [hook(r), pattern([_, _, _, bool(true)])]).

macro(pnorm/4, all, [-,+,-,/], [hook(r), pattern([_, _, _, bool(false)])]).

% pnorm/5: explicit tail and log.p arguments
macro(pnorm/5, all, [+,-,+,/,/], [hook(r), pattern([_, _, _, bool(true), bool(_)])]).

macro(pnorm/5, all, [-,+,-,/,/], [hook(r), pattern([_, _, _, bool(false), bool(_)])]).

% qnorm/1: Mu = 0, Sd = 1, lower tail
macro(qnorm/1, all, [+], [hook(r)]).

% qnorm/3: lower.tail = TRUE
interval_(qnorm(number(P), number(Mu), number(Sigma)), Res, _Flags) :-
    qnorm_(P, Mu, Sigma, true, false, Res0),
    !, Res = number(Res0).

qnorm_(P, Mu, Sigma, Tail, LogP, Res) :-
    eval(r(qnorm(P, Mu, Sigma, 'lower.tail'=Tail, 'log.p'=LogP)), Res).

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2), Res, _Flags) :-
    qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, false, Res0),
    !, Res = Res0.

% p < 0.5: [+, +, -] (lower tail)
qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, false, Res) :-
    eval(P2 < 0.5),
    qnorm_(P1, Mu1, Sigma2, true, false, L),
    qnorm_(P2, Mu2, Sigma1, true, false, U),
    !, Res = L...U.

qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, true, Res) :-
    eval(exp(P2) < 0.5),
    qnorm_(P1, Mu1, Sigma2, true, true, L),
    qnorm_(P2, Mu2, Sigma1, true, true, U),
    !, Res = L...U.
    
% p >= 0.5 || p1 =< 0.5, p2 >= 0.5: [+, +, +] (lower tail)
qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, LogP, Res) :-
    qnorm_(P1, Mu1, Sigma1, true, LogP, L),
    qnorm_(P2, Mu2, Sigma2, true, LogP, U),
    !, Res = L...U.

macro(qnorm/3, interval_, []).

% qnorm/4: explicit tail argument
interval_(qnorm(number(P), number(Mu), number(Sigma), bool(Tail)), Res, _Flags) :-
    qnorm_(P, Mu, Sigma, Tail, false, Res0),
    !, Res = number(Res0).

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2, bool(true)), Res, Flags) :-
    interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2), Res0, Flags),
    !, Res = Res0.

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2, bool(false)), Res, _Flags) :-
    qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, false, Res0),
    !, Res = Res0.

% p < 0.5: [-, +, +] (upper tail)
qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, false, Res) :-
    eval(P2 < 0.5),
    qnorm_(P2, Mu1, Sigma1, false, false, L),
    qnorm_(P1, Mu2, Sigma2, false, false, U),
    !, Res = L...U.

qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, true, Res) :-
    eval(exp(P2) < 0.5),
    qnorm_(P2, Mu1, Sigma1, false, true, L),
    qnorm_(P1, Mu2, Sigma2, false, true, U),
    !, Res = L...U.

% p >= 0.5 || p1 =< 0.5, p2 >= 0.5: [-, +, -] (upper tail) 
qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, LogP, Res) :-
    qnorm_(P2, Mu1, Sigma2, false, LogP, L),
    qnorm_(P1, Mu2, Sigma1, false, LogP, U),
    !, Res = L...U.

macro(qnorm/4, interval_, [], [pattern([_, _, _, bool(_)])]).

% qnorm/5: explicit tail and log.p argument
interval_(qnorm(number(P), number(Mu), number(Sigma), bool(Tail), bool(LogP)), Res, _Flags) :-
    qnorm_(P, Mu, Sigma, Tail, LogP, Res0),
    !, Res = number(Res0).

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2, bool(true), bool(LogP)), Res, _Flags) :-
    qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, LogP, Res0),
    !, Res = Res0.

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2, bool(false), bool(LogP)), Res, _Flags) :-
    qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, LogP, Res0),
    !, Res = Res0.

macro(qnorm/5, interval_, [], [pattern([_, _, _, bool(_), bool(_)])]).

% dnorm/1: Mu = 0, Sd = 1, log = FALSE
interval_(dnorm(number(A)), Res, _Flags) :-
    dnorm_(A, false, Res0),
    !, Res = number(Res0).

dnorm_(A, Log, Res) :-
    eval(r(dnorm(A, 'log'=Log)), Res).

interval_(dnorm(L...U), Res, _Flags) :-
    dnorm0(L...U, false, Res0),
    !, Res = Res0.

dnorm0(A...B, Log, Res) :-
    negative(A, B),
    dnorm_(A, Log, L),
    dnorm_(B, Log, U),
    !, Res = L...U.

dnorm0(A...B, Log, Res) :-
    positive(A, B),
    dnorm_(A, Log, U),
    dnorm_(B, Log, L),
    !, Res = L...U.

dnorm0(A...B, Log, Res) :-
    eval(max(abs(A), B), Max),
    dnorm0(0...Max, Log, Res).

% dnorm/2: log argument
interval_(dnorm(number(A), bool(Log)), Res, _Flags) :-
    dnorm_(A, Log, Res0),
    !, Res = number(Res0).

interval_(dnorm(L...U), Log, Res, _Flags) :-
    dnorm0(L...U, Log, Res0),
    !, Res = Res0.

% dnorm/3: mu and sd argument
interval_(dnorm(number(A), number(Mu), number(Sigma)), Res, _Flags) :-
    dnorm_(A, Mu, Sigma, false, Res0),
    !, Res = number(Res0).

dnorm_(A, Mu, Sigma, Log, Res) :-
    eval(r(dnorm(A, Mu, Sigma, 'log'=Log)), Res).

interval_(dnorm(A1...A2, Mu1...Mu2, Sigma1...Sigma2), Res, Flags) :-
    interval_((A1...A2 - Mu1...Mu2) / Sigma1...Sigma2, Z, Flags),
    dnorm0(Z, false, Res0),
    interval_(number(1)/Sigma1...Sigma2 * Res0, Res1, Flags),
    !, Res = Res1.

macro(dnorm/3, interval_, []).

% dnorm/4: mu, sd, log argument
interval_(dnorm(number(A), number(Mu), number(Sigma), bool(Log)), Res, _Flags) :-
    dnorm_(A, Mu, Sigma, Log, Res0),
    !, Res = number(Res0).

interval_(dnorm(A1...A2, Mu1...Mu2, Sigma1...Sigma2, bool(false)), Res, Flags) :-
    !, interval_(dnorm(A1...A2, Mu1...Mu2, Sigma1...Sigma2), Res, Flags).

interval_(dnorm(A1...A2, Mu1...Mu2, Sigma1...Sigma2, bool(true)), Res, Flags) :-
    interval_((A1...A2 - Mu1...Mu2) / Sigma1...Sigma2, Z, Flags),
    dnorm0(Z, false, Res0),
    interval_(number(1)/Sigma1...Sigma2 * Res0, L0...U0, Flags),
    eval(log(L0), log(U0), Res1),
    !, Res = Res1.

macro(dnorm/4, interval_, [], [pattern([_, _, _, bool(_)])]).
%
% t distribution
%
% pt/2: lower.tail = TRUE, log.p = FALSE
macro(pt/2, all, [+,+], [hook(r)]).

% pt/3 log.p = FALSE
interval_(pt(number(A), number(Df), bool(Tail)), Res, _Flags) :-
    pt_(A, Df, Tail, false, Res0),
    !, Res = number(Res0).

pt_(A, Df, Tail, LogP, Res) :-
    eval(r(pt(A, Df, 'lower.tail'=Tail, 'log.p'=LogP)), Res).

interval_(pt(A1...A2, Df1...Df2, bool(Tail)), Res, _Flags) :-
    pt0(A1...A2, Df1...Df2, Tail, false, Res0),
    !, Res = Res0.

pt0(A1...A2, Df1...Df2, true, LogP, L...U) :-
    negative(A1, A2),
    !,
    pt_(A1, Df2, true, LogP, L),
    pt_(A2, Df1, true, LogP, U).

pt0(A1...A2, Df1...Df2, true, LogP, L...U) :-
    positive(A1, A2),
    !,
    pt_(A1, Df1, true, LogP, L),
    pt_(A2, Df2, true, LogP, U).

pt0(A1...A2, Df1...Df2, true, LogP, Res) :-
    eval(max(abs(A1), A2), Max),
    pt0(0...Max, Df1...Df2, true, LogP, Res0),
    !, Res = Res0.

pt0(A1...A2, Df1...Df2, false, LogP, L...U) :-
    negative(A1, A2),
    !,
    pt_(A2, Df1, false, LogP, L),
    pt_(A1, Df2, false, LogP, U).

pt0(A1...A2, Df1...Df2, false, LogP, L...U) :-
    positive(A1, A2),
    !,
    pt_(A2, Df2, false, LogP, L),
    pt_(A1, Df1, false, LogP, U).

pt0(A1...A2, Df1...Df2, false, LogP, Res) :-
    eval(max(abs(A1), A2), Max),
    pt0(0...Max, Df1...Df2, false, LogP, Res0),
    !, Res = Res0.    
    
macro(pt/3, interval_, [], [pattern([_, _, bool(_)])]).

% pt/4 
interval_(pt(number(A), number(Df), bool(Tail), bool(LogP)), Res, _Flags) :-
    pt_(A, Df, Tail, LogP, Res0),
    !, Res = number(Res0).

interval_(pt(A1...A2, Df1...Df2, bool(Tail), bool(LogP)), Res, _Flags) :-
    pt0(A1...A2, Df1...Df2, Tail, LogP, Res0),
    !, Res = Res0.

macro(pt/4, interval_, [], [pattern([_, _, bool(_), bool(_)])]).

% qt/2: lower.tail = TRUE, log.p = TRUE
interval_(qt(number(P), number(Df)), Res, _Flags) :-
    qt_(P, Df, true, false, Res0),
    !, Res = number(Res0).

qt_(P, Df, Tail, LogP, Res) :-
    eval(r(qt(P, Df, 'lower.tail'=Tail, 'log.p'=LogP)), Res).

interval_(qt(P1...P2, Df1...Df2), Res, _Flags) :-
    qt0(P1...P2, Df1...Df2, true, false, Res0),
    !, Res = Res0.

macro(qt/2, interval_, []).

% qt/3 log.p = TRUE
interval_(qt(number(P), number(Df), bool(Tail)), Res, _Flags) :-
    qt_(P, Df, Tail, false, Res0),
    !, Res = number(Res0).

interval_(qt(P1...P2, Df1...Df2, bool(Tail)), Res, _Flags) :-
    qt0(P1...P2, Df1...Df2, Tail, false, Res0),
    !, Res = Res0.

qt0(P1...P2, Df1...Df2, true, false, L...U) :-
    eval(P1 >= 0.5),
    !,
    qt_(P1, Df2, true, false, L),
    qt_(P2, Df1, true, false, U).

qt0(P1...P2, Df1...Df2, true, true, L...U) :-
    eval(exp(P1) >= 0.5),
    !,
    qt_(P1, Df2, true, true, L),
    qt_(P2, Df1, true, true, U).

qt0(P1...P2, Df1...Df2, true, LogP, L...U) :-
    !,
    qt_(P1, Df1, true, LogP, L),
    qt_(P2, Df2, true, LogP, U).

qt0(P1...P2, Df1...Df2, false, false, L...U) :-
    eval(P1 >= 0.5),
    !,
    qt_(P2, Df1, false, false, L),
    qt_(P1, Df2, false, false, U).
    
qt0(P1...P2, Df1...Df2, false, true, L...U) :-
    eval(exp(P1) >= 0.5),
    !,
    qt_(P2, Df1, false, true, L),
    qt_(P1, Df2, false, true, U).

qt0(P1...P2, Df1...Df2, false, LogP, L...U) :-
    !,
    qt_(P2, Df2, false, LogP, L),
    qt_(P1, Df1, false, LogP, U).

macro(qt/3, interval_, [], [pattern([_, _, bool(_)])]).

% qt/4
interval_(qt(number(P), number(Df), bool(Tail), bool(LogP)), Res, _Flags) :-
    qt_(P, Df, Tail, LogP, Res0),
    !, Res = number(Res0).

interval_(qt(P1...P2, Df1...Df2, bool(Tail), bool(LogP)), Res, _Flags) :-
    qt0(P1...P2, Df1...Df2, Tail, LogP, Res0),
    !, Res = Res0.

macro(qt/4, interval_, [], [pattern([_, _, bool(_), bool(_)])]).

% dt/2: log = FALSE
interval_(dt(number(A), number(Df)), Res, _Flags) :-
    dt_(A, Df, false, Res0),
    !, Res = number(Res0).

dt_(A, Df, Log, Res) :-
    eval(r(dt(A, Df, log=Log), Res)).

interval_(dt(A1...A2, Df1...Df2), Res, _Flags) :-
    dt0(A1...A2, Df1...Df2, false, Res0),
    !, Res = Res0.
    
dt0(A1...A2, Df1...Df2, Log, Res) :-
    negative(A1, A2),
    dt_(A1, Df1, Log, L),
    dt_(A2, Df2, Log, U),
    !, Res = L...U.

dt0(A1...A2, Df1...Df2, Log, Res) :-
    positive(A1, A2),
    dt_(A2, Df1, Log, L),
    dt_(A1, Df2, Log, U),
    !, Res = L...U.

dt0(A1...A2, Df1...Df2, Log, Res) :-
    eval(max(abs(A1), A2), Max),
    dt0(0...Max, Df1...Df2, Log, Res0),
    !, Res = Res0.

macro(dt/2, interval_, []).

% dt/3
interval_(dt(number(A), number(Df), bool(Log)), Res, _Flags) :-
    dt_(A, Df, Log, Res0),
    !, Res = number(Res0).

interval_(dt(A1...A2, Df1...Df2, bool(Log)), Res, _Flags) :-
    dt0(A1...A2, Df1...Df2, Log, Res0),
    !, Res = Res0.

macro(dt/3, interval_, [], [pattern([_, _, bool(_)])]).

%
% Chi-squared distribution
%
% pchisq/2: lower.tail = TRUE
macro(pchisq/2, all, [+,-], [hook(r)]).

% pchisq/3: log.p = FALSE
interval_(pchisq(number(A), number(Df), bool(Tail)), Res, _Flags) :-
    pchisq_(A, Df, Tail, Res0),
    !, Res = number(Res0).

pchisq_(A, Df, Tail, Res) :-
    eval(r(pchisq(A, Df, 'lower.tail'=Tail)), Res).

macro(pchisq/3, pchisq_, [+, -, /], [pattern([_, _, bool(true)])]).
macro(pchisq/3, pchisq_, [-, +, /], [pattern([_, _, bool(false)])]).

% pchisq/4
interval_(pchisq(number(A), number(Df), bool(Tail), bool(LogP)), Res, _Flags) :-
    pchisq_(A, Df, Tail, LogP, Res0),
    !, Res = number(Res0).

pchisq_(A, Df, Tail, LogP, Res) :-
    eval(r(pchisq(A, Df, 'lower.tail'=Tail, 'log.p'=LogP)), Res).

macro(pchisq/4, pchisq_, [+, -, /, /], [pattern([_, _, bool(true), bool(_)])]).
macro(pchisq/4, pchisq_, [-, +, /, /], [pattern([_, _, bool(false), bool(_)])]).

% qchisq/2: lower.tail = TRUE
macro(qchisq/2, all, [+,+], [hook(r)]).

% qchisq/3: log.p = FALSE
interval_(qchisq(number(P), number(Df), bool(Tail)), Res, _Flags) :-
    qchisq_(P, Df, Tail, Res0),
    !, Res = number(Res0).

qchisq_(P, Df, Tail, Res) :-
    eval(r(qchisq(P, Df, 'lower.tail'=Tail)), Res).

macro(qchisq/3, qchisq_, [+, +, /], [pattern([_, _, bool(true)])]).
macro(qchisq/3, qchisq_, [-, +, /], [pattern([_, _, bool(false)])]).

% qchisq/4
interval_(qchisq(number(P), number(Df), bool(Tail), bool(LogP)), Res, _Flags) :-
    qchisq_(P, Df, Tail, LogP, Res0),
    !, Res = number(Res0).

qchisq_(P, Df, Tail, LogP, Res) :-
    eval(r(qchisq(P, Df, 'lower.tail'=Tail, 'log.p'=LogP)), Res).

macro(qchisq/4, qchisq_, [+, +, /, /], [pattern([_, _, bool(true), bool(_)])]).
macro(qchisq/4, qchisq_, [-, +, /, /], [pattern([_, _, bool(false), bool(_)])]).

% dchisq/2: log = FALSE
interval_(dchisq(number(A), number(Df)), Res, _Flags) :-
    dchisq_(A, Df, false, Res0),
    !, Res = number(Res0).

dchisq_(A, Df, Log, Res) :-
    eval(r(dchisq(A, Df, log=Log)), Res).

interval_(dchisq(A1...A2, Df1...Df2), Res, _Flags) :-
    dchisq0_(A1...A2, Df1...Df2, false, Res0),
    !, Res = Res0.

% for df2 =< 2
dchisq0_(A1...A2, Df1...Df2, Log, Res) :-
    eval(Df2 =< 2),
    dchisq_(A1, Df1, Log, X1),
    dchisq_(A1, Df2, Log, X2),
    dchisq_(A2, Df1, Log, X3),
    dchisq_(A2, Df2, Log, X4),
    min_list([X1, X2, X3, X4], L),
    max_list([X1, X2, X3, X4], U),
    !, Res = L...U.

% for df1 == 2, df2 > 2, L < mode, U > mode
dchisq0_(A1...A2, 2...Df2, Log, Res) :-
    dif(Df2, 2),
    eval(Df2 - 2, Mode),
    eval(A1 < Mode),
    eval(A2 > Mode),
    dchisq_(A1, 2, Log, X1),
    dchisq_(A2, 2, Log, X2),
    dchisq_(A1, Df2, Log, X3),
    dchisq_(A2, Df2, Log, X4),
    dchisq_(Mode, Df2, Log, U),
    min_list([X1, X2, X3, X4], L),
    max_list([X1, X2, X3, X4, Mode], U),
    !, Res = L...U.

% for df1 == 2, df2 > 2
dchisq0_(A1...A2, 2...Df2, Log, Res) :-
    dif(Df2, 2),
    dchisq_(A1, 2, Log, X1),
    dchisq_(A2, 2, Log, X2),
    dchisq_(A1, Df2, Log, X3),
    dchisq_(A2, Df2, Log, X4),
    min_list([X1, X2, X3, X4], L),
    max_list([X1, X2, X3, X4], U),
    !, Res = L...U.

% for df1 > 2, x < mode
dchisq0_(A1...A2, Df1...Df2, Log, Res) :-
    eval(Df1 - 2, Mode),
    eval(A2 =< Mode),
    dchisq_(A1, Df2, Log, L),
    dchisq_(A2, Df1, Log, U),
    !, Res = L...U.

% for df1 > 2, x > mode
dchisq0_(A1...A2, Df1...Df2, Log, Res) :-
    eval(Df2 - 2, Mode),
    eval(A1 >= Mode),
    dchisq_(A2, Df2, Log, L),
    dchisq_(A1, Df1, Log, U),
    !, Res = L...U.

% for df1 > 2, L < mode, U > mode
dchisq0_(A1...A2, Df1...Df2, Log, Res) :-
    dchisq_(A1, Df2, Log, L0),
    dchisq_(A2, Df1, Log, U0),
    eval(min(L0, U0), L),
    eval(Df1 - 2, Mode),
    dchisq_(Mode, Df1, Log, U),
    !, Res = L...U.

macro(dchisq/2, interval_, []).

% dchisq/3
interval_(dchisq(number(A), number(Df), bool(Log)), Res, _Flags) :-
    dchisq_(A, Df, Log, Res0),
    !, Res = number(Res0).

interval_(dchisq(A1...A2, Df1...Df2, bool(Log)), Res, _Flags) :-
    dchisq0_(A1...A2, Df1...Df2, Log, Res0),
    !, Res = Res0.

macro(dchisq/3, interval_, [], [pattern([_, _, bool(_)])]).