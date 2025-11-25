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

% qbinom/3: default lower.tail = TRUE
macro(qbinom/3, all, [+, +, +], [hook(r)]).

% qbinom/4: explicit tail argument
macro(qbinom/4, all, [+, +, +, /], [hook(r), pattern([_, _, _, bool(true)])]).

macro(qbinom/4, all, [-, +, +, /], [hook(r), pattern([_, _, _, bool(false)])]).

% dbinom
interval_(dbinom(number(Alpha), number(N), number(P)), Res, _Flags) :-
    dbinom_(Alpha, N, P, Res0),
    !, Res = number(Res0).

dbinom_(Alpha, N, P, Res) :-
    eval(r(dbinom(Alpha, N, P)), Res).

interval_(dbinom(X1...X2, N1...N2, P1...P2), Res, _Flags) :-
    dbinom0(X1...X2, N1...N2, P1...P2, Res0),
    !, Res = Res0.

% left to X / N: [+, -, -]
dbinom0(X1...X2, N1...N2, P1...P2, L...U) :-
    eval(X2 < N1 * P1),
    !,
    dbinom_(X1, N2, P2, L),
    dbinom_(X2, N1, P1, U).

% right to X / N: [-, +, +]
dbinom0(X1...X2, N1...N2, P1...P2, L...U) :-
    eval(X1 > N2 * P2),
    !,
    dbinom_(X2, N1, P1, L),
    dbinom_(X1, N2, P2, U).

% otherwise
dbinom0(K1...K2, N1...N2, P1...P2, L...U) :-
    eval(r(g <- 'expand.grid'('k'=K1:K2, 'N'=N1:N2)), _),
    eval(r(g <- subset(g, '&'('$'(g, k) >= floor(P1 * '$'(g, 'N')), '<='('$'(g, k), ceiling(P2 * '$'(g, 'N')))))), _),
    eval(r(c(min(dbinom(c(K1, K2), c(N2, N1), c(P2, P1))), 
            max(dbinom('$'(g, k), '$'(g, 'N'), ifelse('$'(g, k) > '$'(g, 'N') * P2, P2, 
                ifelse('$'(g, k) < '$'(g, 'N') * P1, P1, '$'(g, k)/'$'(g, 'N'))))))), ##(L, U)).

macro(dbinom/3, interval_, []).

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

% qnorm/1: Mu = 0, Sd = 1, lower tail
macro(qnorm/1, all, [+], [hook(r)]).

% qnorm/3: lower.tail = TRUE
interval_(qnorm(number(P), number(Mu), number(Sigma)), Res, _Flags) :-
    qnorm_lower(P, Mu, Sigma, Res0),
    !, Res = number(Res0).

qnorm_lower(P, Mu, Sigma, Res) :-
    eval(r(qnorm(P, Mu, Sigma, 'lower.tail'=true)), Res).

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2), Res, _Flags) :-
    qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, Res0),
    !, Res = Res0.

% p < 0.5: [+, +, -] (lower tail)
qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, Res) :-
    eval(P2 < 0.5),
    qnorm_lower(P1, Mu1, Sigma2, L),
    qnorm_lower(P2, Mu2, Sigma1, U),
    !, Res = L...U.
    
% p >= 0.5 || p1 =< 0.5, p2 >= 0.5: [+, +, +] (lower tail)
qnorm0(P1...P2, Mu1...Mu2, Sigma1...Sigma2, Res) :-
    qnorm_lower(P1, Mu1, Sigma1, L),
    qnorm_lower(P2, Mu2, Sigma2, U),
    !, Res = L...U.

macro(qnorm/3, interval_, []).

% qnorm/4: explicit tail argument
interval_(qnorm(number(P), number(Mu), number(Sigma), bool(true)), Res, _Flags) :-
    qnorm_lower(P, Mu, Sigma, Res0),
    !, Res = number(Res0).

interval_(qnorm(number(P), number(Mu), number(Sigma), bool(false)), Res, _Flags) :-
    qnorm_upper(P, Mu, Sigma, Res0),
    !, Res = number(Res0).

qnorm_upper(P, Mu, Sigma, Res) :-
    eval(r(qnorm(P, Mu, Sigma, 'lower.tail'=false)), Res).

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2, bool(true)), Res, Flags) :-
    interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2), Res0, Flags),
    !, Res = Res0.

interval_(qnorm(P1...P2, Mu1...Mu2, Sigma1...Sigma2, bool(false)), Res, _Flags) :-
    qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, Res0),
    !, Res = Res0.

% p < 0.5: [-, +, +] (upper tail)
qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, Res) :-
    eval(P2 < 0.5),
    qnorm_upper(P2, Mu1, Sigma1, L),
    qnorm_upper(P1, Mu2, Sigma2, U),
    !, Res = L...U.

% p >= 0.5 || p1 =< 0.5, p2 >= 0.5: [-, +, -] (upper tail) 
qnorm1(P1...P2, Mu1...Mu2, Sigma1...Sigma2, Res) :-
    qnorm_upper(P2, Mu1, Sigma2, L),
    qnorm_upper(P1, Mu2, Sigma1, U),
    !, Res = L...U.

macro(qnorm/4, interval_, [], [pattern([_, _, _, bool(true)])]).

macro(qnorm/4, interval_, [], [pattern([_, _, _, bool(false)])]).

% dnorm/1: Mu = 0, Sd = 1
interval_(dnorm(number(A)), Res, _Flags) :-
    dnorm_(A, Res0),
    !, Res = number(Res0).

dnorm_(A, Res) :-
    eval(r(dnorm(A)), Res).

interval_(dnorm(L...U), Res, _Flags) :-
    dnorm0(L...U, Res0),
    !, Res = Res0.

dnorm0(A...B, Res) :-
    negative(A, B),
    dnorm_(A, L),
    dnorm_(B, U),
    !, Res = L...U.

dnorm0(A...B, Res) :-
    positive(A, B),
    dnorm_(A, U),
    dnorm_(B, L),
    !, Res = L...U.

dnorm0(A...B, Res) :-
    eval(max(abs(A), B), Max),
    dnorm0(0...Max, Res).

% dnorm/3
interval_(dnorm(number(A), number(Mu), number(Sigma)), Res, _Flags) :-
    dnorm_(A, Mu, Sigma, Res0),
    !, Res = number(Res0).

dnorm_(A, Mu, Sigma, Res) :-
    eval(r(dnorm(A, Mu, Sigma)), Res).

interval_(dnorm(A1...A2, Mu1...Mu2, Sigma1...Sigma2), Res, Flags) :-
    interval_((A1...A2 - Mu1...Mu2) / Sigma1...Sigma2, Z, Flags),
    dnorm0(Z, Res0),
    interval_(number(1)/Sigma1...Sigma2 * Res0, Res1, Flags),
    !, Res = Res1.

macro(dnorm/3, interval_, []).

%
% t distribution
%
% pt/2: lower.tail = TRUE
macro(pt/2, all, [+,+], [hook(r)]).

% pt/3
interval_(pt(number(A), number(Df), bool(true)), Res, _Flags) :-
    pt_lower(A, Df, Res0),
    !, Res = number(Res0).

interval_(pt(number(A), number(Df), bool(false)), Res, _Flags) :-
    pt_upper(A, Df, Res0),
    !, Res = number(Res0).

pt_lower(A, Df, Res) :-
    eval(r(pt(A, Df, 'lower.tail'=true)), Res).

pt_upper(A, Df, Res) :-
    eval(r(pt(A, Df, 'lower.tail'=false)), Res).

interval_(pt(A1...A2, Df1...Df2, bool(Tail)), Res, _Flags) :-
    pt_(A1...A2, Df1...Df2, Tail, Res0),
    !, Res = Res0.

pt_(A1...A2, Df1...Df2, true, L...U) :-
    negative(A1, A2),
    !,
    pt_lower(A1, Df2, L),
    pt_lower(A2, Df1, U).

pt_(A1...A2, Df1...Df2, true, L...U) :-
    positive(A1, A2),
    !,
    pt_lower(A1, Df1, L),
    pt_lower(A2, Df2, U).

pt_(A1...A2, Df1...Df2, true, Res) :-
    eval(max(abs(A1), A2), Max),
    pt_(0...Max, Df1...Df2, true, Res0),
    !, Res = Res0.

pt_(A1...A2, Df1...Df2, false, L...U) :-
    negative(A1, A2),
    !,
    pt_upper(A2, Df1, L),
    pt_upper(A1, Df2, U).

pt_(A1...A2, Df1...Df2, false, L...U) :-
    positive(A1, A2),
    !,
    pt_upper(A2, Df2, L),
    pt_upper(A1, Df1, U).

pt_(A1...A2, Df1...Df2, false, Res) :-
    eval(max(abs(A1), A2), Max),
    pt_(0...Max, Df1...Df2, false, Res0),
    !, Res = Res0.    
    
macro(pt/3, interval_, [], [pattern([_, _, bool(_)])]).

% qt/2: lower.tail = TRUE
interval_(qt(number(P), number(Df)), Res, _Flags) :-
    qt_lower(P, Df, Res0),
    !, Res = number(Res0).

qt_lower(P, Df, Res) :-
    eval(r(qt(P, Df, 'lower.tail'=true)), Res).

interval_(qt(P1...P2, Df1...Df2), Res, _Flags) :-
    qt_(P1...P2, Df1...Df2, true, Res0),
    !, Res = Res0.

macro(qt/2, interval_, []).

% qt/3
interval_(qt(number(P), number(Df), bool(true)), Res, _Flags) :-
    qt_lower(P, Df, Res0),
    !, Res = number(Res0).

interval_(qt(number(P), number(Df), bool(false)), Res, _Flags) :-
    qt_upper(P, Df, Res0),
    !, Res = number(Res0).

qt_upper(P, Df, Res) :-
    eval(r(qt(P, Df, 'lower.tail'=false)), Res).

interval_(qt(P1...P2, Df1...Df2, bool(Tail)), Res, _Flags) :-
    qt_(P1...P2, Df1...Df2, Tail, Res0),
    !, Res = Res0.

qt_(P1...P2, Df1...Df2, true, L...U) :-
    eval(P1 >= 0.5),
    !,
    qt_lower(P1, Df2, L),
    qt_lower(P2, Df1, U).

qt_(P1...P2, Df1...Df2, true, L...U) :-
    !,
    qt_lower(P1, Df1, L),
    qt_lower(P2, Df2, U).

qt_(P1...P2, Df1...Df2, false, L...U) :-
    eval(P1 >= 0.5),
    !,
    qt_upper(P2, Df1, L),
    qt_upper(P1, Df2, U).

qt_(P1...P2, Df1...Df2, false, L...U) :-
    !,
    qt_upper(P2, Df2, L),
    qt_upper(P1, Df1, U).

macro(qt/3, interval_, [], [pattern([_, _, bool(_)])]).

% dt/2
interval_(dt(number(A), number(Df)), Res, _Flags) :-
    dt_(A, Df, Res0),
    !, Res = number(Res0).

dt_(A, Df, Res) :-
    eval(r(dt(A, Df), Res)).

interval_(dt(A1...A2, Df1...Df2), Res, _Flags) :-
    dt0(A1...A2, Df1...Df2, Res0),
    !, Res = Res0.
    
dt0(A1...A2, Df1...Df2, Res) :-
    negative(A1, A2),
    dt_(A1, Df1, L),
    dt_(A2, Df2, U),
    !, Res = L...U.

dt0(A1...A2, Df1...Df2, Res) :-
    positive(A1, A2),
    dt_(A2, Df1, L),
    dt_(A1, Df2, U),
    !, Res = L...U.

dt0(A1...A2, Df1...Df2, Res) :-
    eval(max(abs(A1), A2), Max),
    dt0(0...Max, Df1...Df2, Res0),
    !, Res = Res0.

macro(dt/2, interval_, []).

%
% Chi-squared distribution
%
% pchisq/2: lower.tail = TRUE
macro(pchisq/2, all, [+,-], [hook(r)]).

% pchisq/3
interval_(pchisq(number(A), number(Df), bool(true)), Res, _Flags) :-
    pchisq_lower(A, Df, true, Res0),
    !, Res = number(Res0).

interval_(pchisq(number(A), number(Df), bool(false)), Res, _Flags) :-
    pchisq_upper(A, Df, false, Res0),
    !, Res = number(Res0).

pchisq_lower(A, Df, Tail, Res) :-
    eval(r(pchisq(A, Df, 'lower.tail'=Tail)), Res).

pchisq_upper(A, Df, Tail, Res) :-
    eval(r(pchisq(A, Df, 'lower.tail'=Tail)), Res).

macro(pchisq/3, pchisq_lower, [+, -, /], [pattern([_, _, bool(true)])]).
macro(pchisq/3, pchisq_upper, [-, +, /], [pattern([_, _, bool(false)])]).

% qchisq/2: lower.tail = TRUE
macro(qchisq/2, all, [+,-], [hook(r)]).

% qchisq/3
interval_(qchisq(number(P), number(Df), bool(true)), Res, _Flags) :-
    qchisq_lower(P, Df, true, Res0),
    !, Res = number(Res0).

interval_(qchisq(number(P), number(Df), bool(false)), Res, _Flags) :-
    qchisq_upper(P, Df, false, Res0),
    !, Res = number(Res0).

qchisq_lower(P, Df, Tail, Res) :-
    eval(r(qchisq(P, Df, 'lower.tail'=Tail)), Res).

qchisq_upper(P, Df, Tail, Res) :-
    eval(r(qchisq(P, Df, 'lower.tail'=Tail)), Res).

macro(qchisq/3, qchisq_lower, [+, +, /], [pattern([_, _, bool(true)])]).
macro(qchisq/3, qchisq_upper, [-, +, /], [pattern([_, _, bool(false)])]).

% dchisq/2
interval_(dchisq(number(A), number(Df)), Res, _Flags) :-
    dchisq_(A, Df, Res0),
    !, Res = number(Res0).

dchisq_(A, Df, Res) :-
    eval(r(dchisq(A, Df)), Res).

interval_(dchisq(A1...A2, Df1...Df2), Res, _Flags) :-
    dchisq0_(A1...A2, Df1...Df2, Res0),
    !, Res = Res0.

% for df<=2
dchisq0_(A1...A2, Df1...Df2, Res) :-
    eval(Df2 =< 2),
    dchisq_(A2, Df2, L),
    dchisq_(A1, Df1, U),
    !, Res = L...U.

% for x < mode
dchisq0_(A1...A2, Df1...Df2, Res) :-
    eval(Df1 - 2, Mode),
    eval(A2 =< Mode),
    dchisq_(A1, Df2, L),
    dchisq_(A2, Df1, U),
    !, Res = L...U.

% for x > mode
dchisq0_(A1...A2, Df1...Df2, Res) :-
    eval(Df2 - 2, Mode),
    eval(A1 >= Mode),
    dchisq_(A2, Df2, L),
    dchisq_(A1, Df1, U),
    !, Res = L...U.

% for L < mode, U > mode
dchisq0_(A1...A2, Df1...Df2, Res) :-
    dchisq_(A1, Df2, L0),
    dchisq_(A2, Df1, U0),
    eval(min(L0, U0), L),
    eval(Df1 - 2, Mode),
    dchisq_(Mode, Df1, U),
    !, Res = L...U.

macro(dchisq/2, interval_, []).
