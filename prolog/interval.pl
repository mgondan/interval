% Interval arithmetic in Prolog
:- module(interval, [ int/2, op(150, xfx, ...), example/0 ]).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

example(1 + 2).
example(1 + 3...4).
example(1...2 + 3...4).
example(1...2 + (3...4 + 5...6)).
example(1...2 - 3...4).
example(1...2 - (3...4 + 5...6)).
example(1...2 * 3...4).
example(1...2 / 3...4).
example(1...2 / -1...1).
example(2.0 * (1...2 / -1...1)).
example(2...3 * (1...2 / -1...1)).
example((2 * 1...2) / -1...1).
example((2...3 * 1...2) / -1...1).

example :-
    example(Expr),
    writeln(Expr),
    int(Expr, Res),
    writeln(Res).

% scalars, symbols like pi
int(X, Res),
    atomic(X)
 => Res = X.

% nested expressions, evaluate components
int(Expr, Res),
    compound(Expr)
 => compound_name_arguments(Expr, Name, Args),
    maplist(int, Args, List),
    compound_name_arguments(Top, Name, List),
    top(Top, Res).

% top-level expressions (do not evaluate their arguments)
top(L ... U, Res)
 => Res = L ... U.

% division is non-deterministic (see below)
top(A / B, Res)
 => div(A, B, Res).

% functions with "mono" support
top(Expr, Res),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Signs)
 => compound_name_arguments(Expr, Name, Args),
    findall(R,
        (   maplist(lower, Signs, Args, Lower),
            compound_name_arguments(LowerExpr, Name, Lower),
            R is LowerExpr
        ), Ls),
    min_list(Ls, L),
    findall(R,
        (   maplist(upper, Signs, Args, Upper),
            compound_name_arguments(UpperExpr, Name, Upper),
            R is UpperExpr
        ), Us),
    max_list(Us, U),
    Res = L...U.

% Select the right bounds for +, -, *
lower(+, A..._, L)
 => L = A.

lower(-, _...B, L)
 => L = B.

lower(*, A...B, L)
 => ( L = A ; L = B).

lower(_, X, L)
 => L = X.

upper(+, _...B, U)
 => U = B.

upper(-, A..._, U)
 => U = A.

upper(*, A...B, U)
 => ( U = A ; U = B).

upper(_, X, U)
 => U = X.

% monotonically behaving functions
mono((+)/1, [+]).
mono((+)/2, [+, +]).

% negative sign and minus operation
mono((-)/1, [-]).
mono((-)/2, [+, -]).

% combine everything with everything
mono((*)/2, [*, *]).

% Hickey Theorem 8 and Figure 4
%
% P1 / P (special case, then general case)
div(A ... B, 0.0 ... D, Res),
    pos1(A ... B),
    pos(0.0 ... D)
 => L is A / D,
    U is 1.0Inf,
    Res = L...U.

div(A ... B, C ... D, Res),
    pos1(A ... B),
    pos(C ... D)
 => L is A / D,
    U is B / C,
    Res = L...U.

% P0 / P
div(A ... B, 0.0 ... D, Res),
    pos0(A ... B),
    pos(0.0 ... D)
 => L is 0.0,
    U is 1.0Inf,
    Res = L...U.

div(A ... B, C ... D, Res),
    pos0(A ... B),
    pos(C ... D)
 => L is 0.0,
    U is B / C,
    Res = L...U.

% M / P
div(A ... B, 0.0 ... D, Res),
    mix(A ... B),
    pos(0.0 ... D)
 => L is -1.0Inf,
    U is 1.0Inf,
    Res = L...U.

div(A ... B, C ... D, Res),
    mix(A ... B),
    pos(C ... D)
 => L is A / C,
    U is B / C,
    Res = L...U.

% N0 / P
div(A ... B, 0.0 ... D, Res),
    neg0(A ... B),
    pos(0.0 ... D)
 => L is -1.0Inf,
    U is 0.0,
    Res = L...U.

div(A ... B, C ... D, Res),
    neg0(A ... B),
    pos(C ... D)
 => L is A / C,
    U is 0.0,
    Res = L...U.

% N1 / P
div(A ... B, 0.0 ... D, Res),
    neg1(A ... B),
    pos(0.0 ... D)
 => L is -1.0Inf,
    U is B / D,
    Res = L...U.

div(A ... B, C ... D, Res),
    neg1(A ... B),
    pos(C ... D)
 => L is A / C,
    U is B / D,
    Res = L...U.

% P1 / M (2 solutions)
div(A ... B, C ... D, Res),
    pos1(A ... B),
    mix(C ... D)
 => (   L is -1.0Inf,
        U is A / C,
        Res = L...U
    ;   L is A / D,
        U is 1.0Inf,
        Res = L...U
    ).

% P0 / M
div(A ... B, C ... D, Res),
    pos0(A ... B),
    mix(C ... D)
 => L is -1.0Inf,
    U is 1.0Inf,
    Res = L...U.

% M / M
div(A ... B, C ... D, Res),
    mix(A ... B),
    mix(C ... D)
 => L is -1.0Inf,
    U is 1.0Inf,
    Res = L...U.

% N0 / M
div(A ... B, C ... D, Res),
    neg0(A ... B),
    mix(C ... D)
 => L is -1.0Inf,
    U is 1.0Inf,
    Res = L...U.

% N1 / M (2 solutions)
div(A ... B, C ... D, Res),
    neg0(A ... B),
    mix(C ... D)
 => (   L is -1.0Inf,
        U is B / D,
        Res = L...U
    ;   L is B / C,
        U is 1.0Inf,
        Res = L...U
    ).

% P1 / N
div(A ... B, C ... 0.0, Res),
    pos1(A ... B),
    neg(C ... 0.0)
 => L is -1.0Inf,
    U is A / C,
    Res = L...U.

div(A ... B, C ... D, Res),
    pos1(A ... B),
    neg(C ... D)
 => L is B / D,
    U is A / C,
    Res = L...U.

% P0 / N
div(A ... B, C ... 0.0, Res),
    pos0(A ... B),
    neg(C ... 0.0)
 => L is -1.0Inf,
    U is 0.0,
    Res = L...U.

div(A ... B, C ... D, Res),
    pos0(A ... B),
    neg(C ... D)
 => L is B / D,
    U is 0.0,
    Res = L...U.

% M / N
div(A ... B, C ... 0.0, Res),
    mix(A ... B),
    neg(C ... 0.0)
 => L is -1.0Inf,
    U is 1.0Inf,
    Res = L...U.

div(A ... B, C ... D, Res),
    mix(A ... B),
    neg(C ... D)
 => L is B / D,
    U is A / D,
    Res = L...U.

% N0 / N
div(A ... B, C ... 0.0, Res),
    neg0(A ... B),
    neg(C ... 0.0)
 => L is 0.0,
    U is 1.0Inf,
    Res = L...U.

div(A ... B, C ... D, Res),
    neg0(A ... B),
    neg(C ... D)
 => L is 0.0,
    U is A / D,
    Res = L...U.

% N1 / N
div(A ... B, C ... 0.0, Res),
    neg1(A ... B),
    neg(C ... 0.0)
 => L is B / C,
    U is 1.0Inf,
    Res = L...U.

div(A ... B, C ... D, Res),
    neg1(A ... B),
    neg(C ... D)
 => L is B / C,
    U is A / D,
    Res = L...U.

%
% Hickey Figure 1
%
mix(L ... U) :-
    L < 0,
    U > 0.

pos(L ... U) :-
    L >= 0,
    U > 0.

pos0(0.0 ... U) :-
    U > 0.

pos1(L ... _) :-
    L > 0.

neg(L ... U) :-
    L < 0,
    U =< 0.

neg0(L ... 0.0) :-
    L < 0.

neg1(_ ... U) :-
    U < 0.




