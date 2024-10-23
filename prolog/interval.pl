:- module(interval, [interval/2, op(150, xfx, ...)]).

:- multifile int_hook/2.
:- multifile eval_hook/2.
:- multifile mono/2.

:- discontiguous interval/2.
:- discontiguous int_hook/2.

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

%% <module> Perform arithmetic operations with intervals.
%
% This module adds interval arithemtic to Prolog. 
% An interval is represented as L...U, where L stands for the lower bound and 
% U the upper bound. If the upper bound is a negative number, it has to be written with 
% an additional space, e.g., 1... -2, or in the infix notation, ...(1, -2).  
% The interval/2 parses and evaluates the arithemtic expression with such intervals
% to a result.

%%  interval(+A, ?Res)
%   Evalutes an expression to an interval. If the first argument is already an interval, no evaluation is performed.
%   Supported operations: 
%     - Basic arithemtic: addition '+', subtraction '-', division '/', multiplication '*'
%     - Square root: 'interval(sqrt(X), Res)'
%     - Power: 'interval(X^N, Res)' with N being a natural number
%     - Absolute value: 'interval(abs(X), Res)'
%     - Comparison: '>', '<', '>=', '=<'
%     - Rounding: 'interval(round(1.356...1.634), Res, [digit(2)])' 
%                  with digit(Dig) as third argument and Dig = number of digits after the comma.
%   
%   @arg A is the expression to be evaluted.
%   @arg Res is the result.

%
% If 1st argument already is an interval, do not do anything
%
interval(L...U, Res)
 => Res = L...U.

% Force translation of atom to interval
/* interval(atomic(A), L...U)
 => eval(A, Res),
    L = Res,
    U = Res. */

interval(atomic(A), Res)
 => eval(A, R),
    Res = atomic(R).

%
% Hook for custom interval functions
%
% 1. Declare function with interval:int_hook(Name, pred_name(Args))
%    Args specify the type of the arguments, ... for interval and atomic for numbers.
% 2. Calculate result with interval:pred_name(Args, Res)
%    Args are the argument types as defined in the hook with variable names.
% 
%
% see below example for (/)/2.
interval(Expr, Res),
    compound(Expr),
    compound_name_arguments(Expr, Name, Args),
    int_hook(Name, Mask),
    compound_name_arguments(Mask, Fun, Args1),
    maplist(instantiate, Args1, Args2),
    maplist(interval, Args, Args2)
 => compound_name_arguments(Goal, Fun, Args2),
    call(Goal, Res).

instantiate(atomic, atomic(_)).
instantiate(..., _..._).

%
% Monotonically behaving functions
%
% +: increasing
% -: decreasing
% *: increasing or decreasing
% **: commutative, all *
% /: symbolic argument or flag
mono((+)/1, [+]).
mono((+)/2, [+, +]).
mono((-)/1, [-]).
mono((-)/2, [+, -]).
mono((*)/2, **).
mono((^)/2, [*, /]).

% special case: multiplication ([*, *], commutative)
interval(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, **)
 => compound_name_arguments(Expr, Name, Args),
    maplist(interval, Args, Args1),
    findall(R, both(Name, Args1, R), Bounds),
    min_list(Bounds, L),
    max_list(Bounds, U),
    Res = L...U.

% general case
interval(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Dir)
 => compound_name_arguments(Expr, Name, Args),
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

both(Name, Args, Res) :-
    maplist(lower(*), Args, Lower),
    Expr =.. [Name | Lower],
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
int_hook(<, less1(atomic, atomic)).

less1(atomic(A), atomic(B), Res) :-
    A < B,
    !,
    Res = true.

less1(atomic(_) < atomic(_), Res) :-
    !,
    Res = false.

int_hook(<, less2(..., ...)).

less2(_...A2, B1..._, Res) :-
    A2 < B1,
    !,
    Res = true.

less2(_..._, _..._, false2).

int_hook(=<, less_eq(..., ...)).

less_eq(A1..._, _...B2, Res) :-
    A1 =< B2,
    !,
    Res = true.

less_eq(_..._, _..._, false).

int_hook(>, great(..., ...)).
great(A1..._, _...B2, Res) :-
    A1 > B2,
    !,
    Res = true.

great(_..._, _..._, false).

int_hook(>=, great_eq(..., ...)).
great_eq(_...A2, B1..._, Res) :-
    A2 >= B1,
    !,
    Res = true.

great_eq(_..._, _..._, false).


int_hook(=/=, not_eq(..., ...)).
not_eq(A...B, C...D, Res) :-
    (   less2(A...B, C...D, true)
    ;   great(A...B, C...D, true)
    ), !,
    Res = true.

not_eq(_..._, _..._, false).


int_hook(=:=, eq(..., ...)).
eq(A...B, C...D, Res) :-
    less_eq(A...B, C...D, true),
    great_eq(A...B, C...D, true),
    !,
    Res = true.

eq(_..._, _..._, false). 

%
% Division
%
int_hook(/, div1(..., ...)).
div1(A...B, C...D, Res) :-
    !,
    div(A...B, C...D, Res).

int_hook(/, div2(..., atomic)).
div2(A1...A2, atomic(B), Res) :-
    !,
    div(A1...A2, B...B, Res).

int_hook(/, div3(atomic, ...)).
div3(atomic(A), B1...B2, Res) :-
    !,
    div(A...A, B1...B2, Res).

int_hook(/, div4(atomic, atomic)).
div4(atomic(A), atomic(B), Res) :-
    Res is A / B.

% Hickey Figure 1
mixed(L, U) :-
    L < 0,
    U > 0.

positive(L, U) :-
    L >= 0,
    U > 0.

zeropos(L, U) :-
    L =:= 0,
    U > 0.

strictpos(L, _) :-
    L > 0.

negative(L, U) :-
    L < 0,
    U =< 0.

zeroneg(L, U) :-
    L < 0,
    U =:= 0.

strictneg(_, U) :-
    U < 0.

%
% Hickey Theorem 8 and Figure 4
%
% P1 / P (special case, then general case)
div(A...B, C...D, Res),
    strictpos(A, B),
    zeropos(C, D)
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

%
% Square root
%
% sqrt/1: "normal" behavior, returns nan for negative argument
% sqrt1/1: crops negative part of interval at 0
%
mono(sqrt/1, [+]).

int_hook(sqrt, sqrt1(atomic)).
sqrt1(atomic(X), Res) :-
    eval(sqrt(X), Res).

int_hook(sqrt, sqrt2(...)).
sqrt2(A...B, Res) :-
    strictneg(A, B),
    !,
    Res = 1.5NaN.

sqrt2(A...B, Res) :-
    zeroneg(A, B),
    !,
    Res = 1.5NaN...0.0.

sqrt2(A...B, Res) :-
    zeropos(A, B),
    !,
    eval(sqrt(B), U),
    Res = 0.0...U.

sqrt2(A...B, Res) :-
    strictpos(A, B),
    !,
    eval(sqrt(A), L),
    eval(sqrt(B), U),
    Res = L...U.

sqrt2(A...B, Res) :-
    mixed(A, B),
    !,
    eval(sqrt(B), U),
    Res = 1.5NaN...U.
%
% Power
%
int_hook((^)/2, []).
int_hook(Base^Exp, Res) :-
    interval(Base, Base1),
    interval(Exp, Exp1),
    power(Base1, Exp1, Res).

% Even exponent with negative base
power(L...U, Exp, Res),
    negative(L, U),
    natural(Exp),
    even(Exp)
 => eval(U^Exp, L^Exp, Res).

% Even exponent with mixed base
power(L...U, Exp, Res),
    mixed(L, U),
    natural(Exp),
    even(Exp)
 => eval(max(L^Exp, U^Exp), Upper),
    Res = 0...Upper.

% General case
power(L...U, Exp, Res),
    natural(Exp)
 => eval(L^Exp, U^Exp, Res).

% Utility
even(A) :-
    A mod 2 =:= 0.

natural(A) :-
    A >=0,
    integer(A).

%
% Absolute value
%
int_hook(abs/1, []).
int_hook(abs(A...B), Res) :-
    positive(A, B),
    !,
    Res = A...B.

int_hook(abs(A...B), Res) :-
    negative(A, B),
    !,
    eval(abs(A), U),
    eval(abs(B), L),
    Res = L...U.

% mixed
int_hook(abs(A...B), Res) :-
    !,
    L = 0.0,
    U is max(abs(A), abs(B)),
    Res = L...U.

%
% round interval
%
int_hook(round/1, []).
int_hook(round(A...B), Res) :-
    option(digit(Dig), _Opt, 2),
    eval(floor(A, Dig), A1),
    eval(ceiling(B, Dig), B1),
    Res = A1...B1.

eval_hook(floor(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is floor(A * Mul) / Mul.

eval_hook(ceiling(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is ceiling(A * Mul) / Mul.

% For convenience
eval(Expr1, Expr2, L ... U) :-
    interval:eval(Expr1, L),
    interval:eval(Expr2, U).
