:- module(interval, [interval/2, op(150, xfx, ...)]).

:- multifile int_hook/3.
:- multifile eval_hook/2.
:- multifile mono/2.

:- discontiguous interval_/2.
:- discontiguous int_hook/3.

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

%% <module> Perform arithmetic operations with intervals.
%
% This module adds interval arithemtic to Prolog. 
% An interval is represented as L...U, where L stands for the lower bound and 
% U for the upper bound. If the upper bound is a negative number, it has to be written with 
% an additional space, e.g., -3... -2, or in the infix notation, ...(-3, -2).  
% The interval/2 parses and evaluates the arithemtic expression with such intervals
% to a result.

%%  interval(+A, ?Res)
%   Evalutes an expression to an interval. If the first argument is already an interval, no evaluation is performed.
%   Supported operations: 
%     - Basic arithemtic: addition '+', subtraction '-', division '/', multiplication '*'
%     - Square root for positive interval: 'interval(sqrt(X), Res)'
%     - Square root for negative or mixed interval: 'interval(sqrt1(X), Res)'
%     - Power: 'interval(X^atomic(N), Res)' with N being a natural number
%     - Absolute value: 'interval(abs(X), Res)'
%     - Comparison: '>', '<', '>=', '=<', '=\=', '=:='
%     - Rounding: 'interval(round(1.356...1.634, atomic(2)), Res)' 
%   
%   @arg A is the expression to be evaluted.
%   @arg Res is the result.

interval(Expr, Res1) :-
    clean(Expr, Expr1),
    interval_(Expr1, Res0),
    unwrap(Res0, Res1).

clean(atomic(A), Res)
 => Res = atomic(A).

clean(L...U, Res)
 => Res = L...U.

clean(Expr, Expr1),
    compound(Expr)
 => mapargs(clean, Expr, Expr1).

clean(A, A1),
    atomic(A)
 => A1 = atomic(A).

unwrap(atomic(A), Res)
 => Res = A.

unwrap(A, Res)
 => Res = A.

%
% Hook for custom interval functions
%
% 1. Declare function with interval:int_hook(Name, pred_name(Args), [Options])
%    'Args' specify the type of the arguments, '...' for intervals and 'atomic' for numbers and logical values.
%    Add 'evaluate(false)' to 'Options' list to skip evaluation of arguments.
% 2. Calculate result with interval:pred_name(Args, Res)
%    'Args' are the argument types as defined in the hook with variable names, e.g., 'L...U', 'atomic(A)'.
%
% see below example for (/)/2.

interval_(atomic(A), Res),
    Res = L...U
 => L = A,
    U = A.

interval_(atomic(A), Res)
 => Res = atomic(A).

interval_(L...U, Res)
 => Res = L...U.

interval_(Expr, Res),
    compound_name_arguments(Expr, Name, Args),
    int_hook(Name, Mask, Opt),
    option(evaluate(true), Opt, true),
    compound_name_arguments(Mask, Fun, Args1),
    maplist(instantiate, Args1, Args2),
    maplist(interval_, Args, Args2)
 => compound_name_arguments(Goal, Fun, Args2),
    call(Goal, Res).


instantiate(atomic, atomic(_)).
instantiate(..., _..._).

% Skipping evaluation of arguments
interval_(Expr, Res),
    compound_name_arguments(Expr, Name, Args),
    int_hook(Name, Mask, Opt),
    option(evaluate(false), Opt, false)
 => compound_name_arguments(Mask, Fun, Args),
    compound_name_arguments(Goal, Fun, Args),
    call(Goal, Res).

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
interval_(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, **)
 => compound_name_arguments(Expr, Name, Args),
    maplist(interval_, Args, Args1),
    findall(R, both(Name, Args1, R), Bounds),
    min_list(Bounds, L),
    max_list(Bounds, U),
    Res = L...U.

% general case
interval_(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Dir)
 => compound_name_arguments(Expr, Name, Args),
    maplist(interval_, Args, Args1),
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

lower(_, atomic(A), L)
 => L = A.

upper(+, _...B, U)
 => U = B.

upper(-, A..._, U)
 => U = A.

upper(*, A...B, U)
 => U = A ; U = B.

upper(_, atomic(A), U)
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
int_hook(<, less1(atomic, atomic), []).

less1(atomic(A), atomic(B), Res) :-
    A < B,
    !,
    Res = true.

less1(atomic(_) < atomic(_), Res) :-
    !,
    Res = false.

int_hook(<, less2(..., ...), []).

less2(_...A2, B1..._, Res) :-
    A2 < B1,
    !,
    Res = true.

less2(_..._, _..._, false2).

int_hook(=<, less_eq(..., ...), []).

less_eq(A1..._, _...B2, Res) :-
    A1 =< B2,
    !,
    Res = true.

less_eq(_..._, _..._, false).

int_hook(>, great(..., ...), []).
great(A1..._, _...B2, Res) :-
    A1 > B2,
    !,
    Res = true.

great(_..._, _..._, false).

int_hook(>=, great_eq(..., ...), []).
great_eq(_...A2, B1..._, Res) :-
    A2 >= B1,
    !,
    Res = true.

great_eq(_..._, _..._, false).


int_hook(=\=, not_eq(..., ...), []).
not_eq(A...B, C...D, Res) :-
    (   less2(A...B, C...D, true)
    ;   great(A...B, C...D, true)
    ), !,
    Res = true.

not_eq(_..._, _..._, false).


int_hook(=:=, eq(..., ...), []).
eq(A...B, C...D, Res) :-
    less_eq(A...B, C...D, true),
    great_eq(A...B, C...D, true),
    !,
    Res = true.

eq(_..._, _..._, false). 

%
% Division
%
int_hook(/, div1(atomic, atomic), []).
div1(atomic(A), atomic(B), atomic(Res)) :-
    Res is A / B.

int_hook(/, div2(..., ...), []).
div2(A...B, C...D, Res) :-
    !,
    div(A...B, C...D, Res).

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

zero(L, U) :-
    L =:= 0,
    U =:= 0.

%
% atomic 0 in numerator or denominator
%
div(A...B, C...D, Res),
    zero(A, B),
    (   negative(C, D)
    ;   mixed(C, D) 
    ;   positive(C, D)
    )
 => Res = atomic(0).

div(A...B, C...D, Res),
    zero(C, D),
    zeropos(A, B)
 => Res = atomic(1.0Inf).

div(A...B, C...D, Res),
    zero(C, D),
    zeroneg(A, B)
 => Res = atomic(-1.0Inf).

div(A...B, C...D, Res),
    zero(C, D),
    mixed(A, B)
 => (   Res = atomic(-1.0Inf)
    ;   Res = atomic(1.0Inf)
    ).

div(A...B, C...D, Res),
    zero(A, B),
    zero(C, D)
 => Res = atomic(1.5NaN).

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

int_hook(sqrt1, sqrt1(...), []).
sqrt1(A...B, Res) :-
    strictneg(A, B),
    !,
    Res = 1.5NaN.

sqrt1(A...B, Res) :-
    zeroneg(A, B),
    !,
    Res = 0.0.

sqrt1(A...B, Res) :-
    mixed(A, B),
    !,
    eval(sqrt(B), U),
    Res = 0.0...U.
%
% Power
%
% Even exponent with negative base
int_hook((^), pow(..., atomic), []).
pow(L...U, atomic(Exp), Res),
    negative(L, U),
    even(Exp),
    natural(Exp)
 => eval(U^Exp, L^Exp, Res).

% Even exponent with mixed base
pow(L...U, atomic(Exp), Res),
    mixed(L, U),
    even(Exp),
    natural(Exp)
 => eval(max(L^Exp, U^Exp), Upper),
    Res = 0...Upper.

% General case
pow(L...U, atomic(Exp), Res),
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
int_hook(abs, abs1(...), []).
abs1(A...B, Res) :-
    positive(A, B),
    !,
    Res = A...B.

abs1(A...B, Res) :-
    negative(A, B),
    !,
    eval(abs(A), U),
    eval(abs(B), L),
    Res = L...U.

% mixed
abs1(A...B, Res) :-
    !,
    L = 0.0,
    U is max(abs(A), abs(B)),
    Res = L...U.

%
% round
%
int_hook(round, round(_, atomic), []).
round(A, atomic(Dig), Res) :-
    (round1(A, Dig, Res)
    ;
    round2(A, Dig, Res)).
%interval
round1(A...B, Dig, Res) :-
    eval(floor(A, Dig), A1),
    eval(ceiling(B, Dig), B1),
    Res = A1...B1.

eval_hook(floor(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is floor(A * Mul) / Mul.

eval_hook(ceiling(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is ceiling(A * Mul) / Mul.
% atomic
round2(atomic(A), Dig, Res) :-
    Mul is 10^Dig,
    Res is round(A*Mul) / Mul.

% For convenience
eval(Expr1, Expr2, L ... U) :-
    interval:eval(Expr1, L),
    interval:eval(Expr2, U).

%
% sine
%

int_hook(sin, sin(...), []).

% interval extends over more than 2 max/mins
sin(A...B, Res) :-
    A1 is A/pi - 1/2,
    B1 is B/pi - 1/2,
    B1 >= ceiling(A1) + 1,
    !,
    Res = -1...1.

% interval extends over 1 max
sin(A...B, Res) :-
    A1 is A / (2*pi) - 1/4,
    B1 is B / (2*pi) - 1/4,
    B1 >= ceiling(A1),
    !,
    L is min(sin(A), sin(B)),
    Res = L...1.

% interval extends over 1 min
sin(A...B, Res) :-
    A1 is A / (2*pi) + 1/4,
    B1 is B / (2*pi) + 1/4,
    B1 >= ceiling(A1),
    !,
    U is max(sin(A), sin(B)),
    Res = -1...U.

% default rising
sin(A...B, Res) :-
    A1 is sin(A),
    B1 is sin(B),
    sort([A1, B1], [L, U]),
    Res = L...U.
