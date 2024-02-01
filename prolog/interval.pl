% Interval arithmetic in Prolog
:- module(interval, [ interval/2, lower/3, upper/3, op(150, xfx, ...) ]).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- begin_tests(wuenic).

test(interval) :-
  interval(1 + 2, 3.0...3.0),
  interval(1...2 + 3...4, 4.0...6.0).

:- end_tests(wuenic).

example(1 + 3...4).
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
example(2...4 =@= 1...3).
example(-2...(-1)^(-1)).
example(-1...2^(-1)).
example(-2...1^(-1)).
example(1...2^(-2)).
example(-2...(-1)^(-2)).
example(-1...2^(-2)).
example(-2...1^(-2)).
example(-2...(-1)^1).
example(-1...2^1).
example(-2...1^1).
example(1...2^2).
example(-2...(-1)^2).
example(-1...2^2).
example(-2...1^2).
example(abs(-0.2 ... -0.1)).
example(abs(0.1 ... 0.2)).
example(abs(-0.2 ... 0.1)).
example(abs(-0.1 ... 0.2)).

example :-
    example(Expr),
    writeln(Expr),
    interval(Expr, Res),
    writeln(Res).

% scalars, symbols like pi, do not change anything
interval(X, Res),
    atomic(X)
 => Res = X.

% already an interval
interval(L...U, Res)
 => Res = L...U.

% nested expressions, evaluate components
interval(Expr, Res),
    compound(Expr)
 => compound_name_arguments(Expr, Name, Args),
    maplist(interval, Args, List),
    compound_name_arguments(Top, Name, List),
    int(Top, Res).

%
% define here how to handle interval functions
%
:- multifile int/2.
int(L...U, Res)
 => Res = L...U.

% division is non-deterministic (see below)
int(A / B, Res)
 => div(A, B, Res).

% equality = overlapping
int(A =@= B, Res)
 => equal(A, B, Res).

int(A^B, Res)
 => power(A, B, Res).

int(abs(A), Res)
 => absolute(A, Res).

% functions with "mono" support
int(Expr, Res),
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

lower(/, X, L)
 => L = X.

lower(_, X, L)
 => L = X.

upper(+, _...B, U)
 => U = B.

upper(-, A..._, U)
 => U = A.

upper(*, A...B, U)
 => ( U = A ; U = B).

upper(/, X, U)
 => U = X.

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

% leave second argument unchanged
mono((**)/2, [*, /]). % for testing

% Hickey Theorem 8 and Figure 4
%
% P1 / P (special case, then general case)
div(A...B, 0.0...D, Res),
    pos1(A...B),
    pos(0.0...D)
 => L is A / D,
    U is 1.0Inf,
    Res = L...U.

div(A...B, C...D, Res),
    pos1(A...B),
    pos(C...D)
 => L is A / D,
    U is B / C,
    Res = L...U.

% P0 / P
div(A...B, 0.0...D, Res),
    pos0(A...B),
    pos(0.0...D)
 => L is 0.0,
    U is 1.0Inf,
    Res = L...U.

div(A...B, C...D, Res),
    pos0(A...B),
    pos(C...D)
 => L is 0.0,
    U is B / C,
    Res = L...U.

% M / P
div(A...B, 0.0...D, Res),
    mix(A...B),
    pos(0.0...D)
 => L is -1.0Inf,
    U is 1.0Inf,
    Res = L...U.

div(A...B, C...D, Res),
    mix(A...B),
    pos(C...D)
 => L is A / C,
    U is B / C,
    Res = L...U.

% N0 / P
div(A...B, 0.0...D, Res),
    neg0(A...B),
    pos(0.0...D)
 => L is -1.0Inf,
    U is 0.0,
    Res = L...U.

div(A...B, C...D, Res),
    neg0(A...B),
    pos(C...D)
 => L is A / C,
    U is 0.0,
    Res = L...U.

% N1 / P
div(A...B, 0.0...D, Res),
    neg1(A...B),
    pos(0.0...D)
 => U is B / D,
    Res = -1.0Inf...U.

div(A...B, C...D, Res),
    neg1(A...B),
    pos(C...D)
 => L is A / C,
    U is B / D,
    Res = L...U.

% P1 / M (2 solutions)
div(A...B, C...D, Res),
    pos1(A...B),
    mix(C...D)
 => (   U is A / C,
        Res = -1.0Inf...U
    ;   L is A / D,
        Res = L...1.0Inf
    ).

% P0 / M
div(A...B, C...D, Res),
    pos0(A...B),
    mix(C...D)
 => Res = -1.0Inf...1.0Inf.

% M / M
div(A...B, C...D, Res),
    mix(A...B),
    mix(C...D)
 => Res = -1.0Inf...1.0Inf.

% N0 / M
div(A...B, C...D, Res),
    neg0(A...B),
    mix(C...D)
 => Res = -1.0Inf...1.0Inf.

% N1 / M (2 solutions)
div(A...B, C...D, Res),
    neg0(A...B),
    mix(C...D)
 => (   U is B / D,
        Res = -1.0Inf...U
    ;   L is B / C,
        Res = L...1.0Inf
    ).

% P1 / N
div(A...B, C...0.0, Res),
    pos1(A...B),
    neg(C...0.0)
 => U is A / C,
    Res = -1.0Inf...U.

div(A...B, C...D, Res),
    pos1(A...B),
    neg(C...D)
 => L is B / D,
    U is A / C,
    Res = L...U.

% P0 / N
div(A...B, C...0.0, Res),
    pos0(A...B),
    neg(C...0.0)
 => Res = -1.0Inf...0.0.

div(A...B, C...D, Res),
    pos0(A...B),
    neg(C...D)
 => L is B / D,
    Res = L...0.0.

% M / N
div(A...B, C...0.0, Res),
    mix(A...B),
    neg(C...0.0)
 => Res = -1.0Inf...1.0Inf.

div(A...B, C...D, Res),
    mix(A...B),
    neg(C...D)
 => L is B / D,
    U is A / D,
    Res = L...U.

% N0 / N
div(A...B, C...0.0, Res),
    neg0(A...B),
    neg(C...0.0)
 => Res = 0.0...1.0Inf.

div(A...B, C...D, Res),
    neg0(A...B),
    neg(C...D)
 => U is A / D,
    Res = 0.0...U.

% N1 / N
div(A...B, C...0.0, Res),
    neg1(A...B),
    neg(C...0.0)
 => L is B / C,
    Res = L...1.0Inf.

div(A...B, C...D, Res),
    neg1(A...B),
    neg(C...D)
 => L is B / C,
    U is A / D,
    Res = L...U.

% overlapping intervals are considered "equal"
equal(A...B, C...D, Res),
    L is max(A, C),
    U is min(B, D),
    L =< U
 => Res = true.

equal(_, _, Res)
 => Res = false.

% power (limited to integer exponents)
power(A...B, C, Res),
    integer(C),
    (   pos(A...B)
    ;   neg(A...B)
    )
 => Ac is A^C,
    Bc is B^C,
    sort([Ac, Bc], [L, U]),
    Res = L...U.

% mixed
power(A...B, C, Res),
    integer(C),
    C > 0,
    1 is C mod 2
 => Ac is A^C,
    Bc is B^C,
    sort([Ac, Bc], [L, U]),
    Res = L...U.

power(_..._, C, Res),
    integer(C),
    C < 0,
    1 is C mod 2
 => Res = -1.0Inf...1.0Inf.

power(A...B, C, Res),
    integer(C),
    C > 0,
    0 is C mod 2
 => U is max(abs(A), B)^C,
    Res = 0.0...U.

power(A...B, C, Res),
    integer(C),
    C < 0,
    0 is C mod 2
 => L is min(abs(A), B)^C,
    Res = L...1.0Inf.

% absolute value
absolute(A, Res),
    pos(A)
 => Res = A.

absolute(A, Res),
    neg(A)
 => int(-A, Res).

absolute(A...B, Res)
    % mixed
 => L is 0.0,
    U is max(-A, B),
    Res = L...U.

% Hickey Figure 1
mix(L...U) :-
    L < 0,
    U > 0.

pos(L...U) :-
    L >= 0,
    U > 0.

pos0(0.0...U) :-
    U > 0.

pos1(L..._) :-
    L > 0.

neg(L...U) :-
    L < 0,
    U =< 0.

neg0(L...0.0) :-
    L < 0.

neg1(_...U) :-
    U < 0.

