:- module(interval, [interval/2, op(150, xfx, ...)]).

:- multifile int_hook/1.
:- multifile int_hook/2.
:- multifile eval_hook/2.
:- multifile mono/2.

:- discontiguous interval/2.

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

test :-
    test(A),
    interval(A, Res),
    writeln(A = Res).

test(1.1 / -1.2... -1.1).
test(1.1...1.2 / -1.2...1.3).
test(1.1...1.2 / 1.2...1.3).
test(1.1 + 1.2...1.3 * 2.1).
test(1.2...1.3 * 2.1...2.4).
test(1.2...1.3 - 2.1...2.4).
test(1.2...1.3 + 2.1...2.4).
test(1.2...1.3).

%
% If 1st argument already is an interval, do not do anything
%
interval(L...U, Res)
 => Res = L...U.

%
% Hook for custom interval functions
%
% 1. Declare function with interval:int_hook(Name/Arity)
% 2. Calculate result with interval:int_hook(Expr, Res)
%
% see below example for (/)/2.
interval(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    int_hook(Name/Arity)
 => compound_name_arguments(Expr, Name, Args),
    maplist(interval, Args, Args1),
    compound_name_arguments(Expr1, Name, Args1),
    int_hook(Expr1, Res).

%
% Default behavior for atoms
%
% force result to interval (deprecated)
interval(A, L...U),
    atomic(A)
 => eval(A, R),
    L = R,
    U = R.

% work with atom
interval(A, Res),
    atomic(A)
 => eval(A, Res).

%
% Monotonically behaving functions
%
% +: increasing
% -: decreasing
% *: increasing or decreasing
% **: commutative, all *
% /: symbolic argument or flag
%
mono((+)/1, [+]).
mono((+)/2, [+, +]).
mono((-)/1, [-]).
mono((-)/2, [+, -]).
mono((*)/2, **).

% special case: multiplication ([*, *], commutative)
interval(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, **)
 => Expr =.. [ Name | Args],
    maplist(interval, Args, Args1),
    findall(R, both(Name, Args1, R), Bounds),
    min_list(Bounds, L),
    max_list(Bounds, U),
    Res = L...U.

both(Name, Args, Res) :-
    maplist(lower(*), Args, Lower),
    Expr =.. [Name | Lower],
    eval(Expr, Res).

% general case
interval(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Dir)
 => Expr =.. [ Name | Args],
    maplist(interval, Args, Args1),
    findall(R, lower(Dir, Name, Args1, R), Lower),
    min_list(Lower, L),
    findall(R, upper(Dir, Name, Args1, R), Upper),
    max_list(Upper, U),
    Res = L...U.

lower(Dir, Name, Args, Res) :-
    maplist(lower, Dir, Args, Lower),
    Expr =.. [Name | Lower],
    eval(Expr, Res).

upper(Dir, Name, Args, Res) :-
    maplist(upper, Dir, Args, Upper),
    Expr =.. [Name | Upper],
    eval(Expr, Res).

% Obtain lower and upper bounds
lower(+, A..._, L)
 => L = A.

lower(-, _...A, L)
 => L = A.

lower(*, A...B, L)
 => L = A ; L = B.

lower(_, A, L) % either / or A not interval
 => L = A.

upper(+, _...B, U)
 => U = B.

upper(-, A..._, U)
 => U = A.

upper(*, A...B, U)
 => U = A ; U = B.

upper(_, A, U)
 => U = A.

%
% Arithmetic functions for single numbers
%
% Define hooks for R functions etc.
%
eval(Expr, Res),
    eval_hook(Expr, R)
 => Res = R.

eval(X, Res)
 => Res is X.

%
% Comparison
%
int_hook((<)/2).
int_hook(_...A2 < B1..._, Res) :-
    A2 < B1,
    !,
    Res = true.

int_hook(_..._ < _..._, false).

int_hook((=<)/2).
int_hook(A1..._ =< _...B2, Res) :-
    A1 =< B2,
    !,
    Res = true.

int_hook(_..._ =< _..._, false).

int_hook((>)/2).
int_hook(A1..._ > _...B2, Res) :-
    A1 > B2,
    !,
    Res = true.

int_hook(_..._ > _..._, false).

int_hook((>=)/2).
int_hook(_...A2 >= B1..._, Res) :-
    A2 >= B1,
    !,
    Res = true.

int_hook(_..._ >= _..._, false).

int_hook(A =\= B, Res) :-
    (   interval(A < B, true)
    ;   interval(A > B, true)
    ), !,
    Res = true.

int_hook(_..._ =\= _..._, false).

int_hook(A =:= B, Res) :-
    interval(A =< B, true),
    interval(A >= B, true),
    !,
    Res = true.

int_hook(_..._ =:= _..._, false).

:- begin_tests(comparison).

test((<)) :-
    A = 1...2,
    B = 3...4,
    interval(A < B, false).

:- end_tests(comparison).

%
% Division
%
int_hook((/)/2).
int_hook(A1...A2 / B1...B2, Res) :-
    div(A1...A2, B1...B2, Res).

int_hook(A1...A2 / B, Res) :-
    div(A1...A2, B...B, Res).

int_hook(A / B1...B2, Res) :-
    div(A...A, B1...B2, Res).

int_hook(A / B, Res) :-
    Res is A / B.

% Hickey Figure 1
mixed(L, U) :-
    L < 0,
    U > 0.

zero(0.0, 0.0).

positive(L, U) :-
    L >= 0,
    U > 0.

zeropos(0.0, U) :-
    U > 0.

strictpos(L, _) :-
    L > 0.

negative(L, U) :-
    L < 0,
    U =< 0.

zeroneg(L, 0.0) :-
    L < 0.

strictneg(_, U) :-
    U < 0.

%
% Hickey Theorem 8 and Figure 4
%
% P1 / P (special case, then general case)
div(A...B, 0.0...D, Res),
    strictpos(A, B),
    positive(0.0, D)
 => eval(A / D, L),
    Res = L...1.0Inf.

div(A...B, C...D, Res),
    strictpos(A, B),
    positive(C, D)
 => eval(A / D, L),
    eval(B / C, U),
    Res = L...U.

% P0 / P
div(A...B, 0.0...D, Res),
    zeropos(A, B),
    positive(0.0, D)
 => Res = 0.0...1.0Inf.

div(A...B, C...D, Res),
    zeropos(A, B),
    positive(C, D)
 => eval(B / C, U),
    Res = 0.0...U.

% M / P
div(A...B, 0.0...D, Res),
    mixed(A, B),
    positive(0.0, D)
 => Res = -1.0Inf...1.0Inf.

div(A...B, C...D, Res),
    mixed(A, B),
    positive(C, D)
 => eval(A / C, L),
    eval(B / C, U),
    Res = L...U.

% N0 / P
div(A...B, 0.0...D, Res),
    zeroneg(A, B),
    positive(0.0, D)
 => Res = -1.0Inf...0.0.

div(A...B, C...D, Res),
    zeroneg(A, B),
    positive(C, D)
 => eval(A / C, L),
    Res = L...0.0.

% N1 / P
div(A...B, 0.0...D, Res),
    strictneg(A, B),
    positive(0.0, D)
 => eval(B / D, U),
    Res = -1.0Inf...U.

div(A...B, C...D, Res),
    strictneg(A, B),
    positive(C, D)
 => eval(A / C, L),
    eval(B / D, U),
    Res = L...U.

% P1 / M (2 solutions)
div(A...B, C...D, Res),
    strictpos(A, B),
    mixed(C, D)
 => (   eval(A / C, U),
        Res = -1.0Inf...U
    ;   eval(A / D, L),
        Res = L...1.0Inf
    ).

% P0 / M
div(A...B, C...D, Res),
    zeropos(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% M / M
div(A...B, C...D, Res),
    mixed(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% N0 / M
div(A...B, C...D, Res),
    zeroneg(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% N1 / M (2 solutions)
div(A...B, C...D, Res),
    strictneg(A, B),
    mixed(C, D)
 => (   eval(B / D, U),
        Res = -1.0Inf...U
    ;   eval(B / C, L),
        Res = L...1.0Inf
    ).

% P1 / N
div(A...B, C...0.0, Res),
    strictpos(A, B),
    negative(C, 0.0)
 => eval(A / C, U),
    Res = -1.0Inf...U.

div(A...B, C...D, Res),
    strictpos(A, B),
    negative(C, D)
 => eval(B / D, L),
    eval(A / C, U),
    Res = L...U.

% P0 / N
div(A...B, C...0.0, Res),
    zeropos(A, B),
    negative(C, 0.0)
 => Res = -1.0Inf...0.0.

div(A...B, C...D, Res),
    zeropos(A, B),
    negative(C, D)
 => eval(B / D, L),
    Res = L...0.0.

% M / N
div(A...B, C...0.0, Res),
    mixed(A, B),
    negative(C, 0.0)
 => Res = -1.0Inf...1.0Inf.

div(A...B, C...D, Res),
    mixed(A, B),
    negative(C, D)
 => eval(B / D, L),
    eval(A / D, U),
    Res = L...U.

% N0 / N
div(A...B, C...0.0, Res),
    zeroneg(A, B),
    negative(C, 0.0)
 => Res = 0.0...1.0Inf.

div(A...B, C...D, Res),
    zeroneg(A, B),
    negative(C, D)
 => eval(A / D, U),
    Res = 0.0...U.

% N1 / N
div(A...B, C...0.0, Res),
    strictneg(A, B),
    negative(C, 0.0)
 => eval(B / C, L),
    Res = L...1.0Inf.

div(A...B, C...D, Res),
    strictneg(A, B),
    negative(C, D)
 => eval(B / C, L),
    eval(A / D, U),
    Res = L...U.

