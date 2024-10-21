:- module(interval, [interval/2, interval/3, op(150, xfx, ...)]).

:- multifile int_hook/2.
:- multifile int_hook/3.
:- multifile eval_hook/2.
:- multifile mono/2.

:- discontiguous interval/3.

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

/** <module> Perform arithmetic operations with intervals.
This module adds interval arithmetic to Prolog. 
An interval is represented as L...U, where L stands for the lower bound and 
U the upper bound. If the upper bound is a negative number, it has to be written with 
an additional space, e.g., -3... -2, or in the infix notation, ...(-3, -2).  
The predicate interval/2 parses and evaluates arithemtic expressions with such
intervals to a result. interval/3 takes a list with options as an additional
argument. 
*/

%%  interval(+A, ?Res)
%   Evalutes an expression to an interval. If the first argument is already an
%   interval, no evaluation is performed.
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

interval(A, Res) :-
    interval(A, Res, []).

% For maplist, the options should be the leading argument
interval_(Opt, A, Res) :-
    interval(A, Res, Opt).

% If the argument already is an interval, do not do anything.
interval(L...U, Res, _)
 => Res = L...U.

% Hook for custom interval functions
% 1. Declare function with interval:int_hook(Name/Arity, Opt)
% 2. Calculate result with interval:int_hook(Expr, Res)
% see example below for (/)/2.
interval(Expr, Res, Opt),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    compound_name_arity(Pattern, Name, Arity),
    int_hook(Pattern, Opt0)
 => append(Opt0, Opt, Opt1),
    mapargs(interval_(Opt1), Expr, Pattern),
    int_hook(Pattern, Res, Opt1).

% If the arguments are known to be already evaluated, skip maplist
interval(Expr, Res, Opt),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    int_hook(Name/Arity, Opt1),
    append(Opt1, Opt, Opt2)
 => int_hook(Expr, Res, Opt2).

% Default behavior for atoms
% Force evaluation to an interval (deprecated)
interval(A, L...U, _),
    atomic(A)
 => eval(A, R),
    L = R,
    U = R.

% Work with atom
interval(A, Res, _),
    atomic(A)
 => eval(A, Res).

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
mono((^)/2, [*, /]).

% Special case: multiplication (commutative)
interval(Expr, Res, Opt),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, **)
 => Expr =.. [Name | Args],
    maplist(interval_(Opt), Args, Args1),
    findall(R, both(Name, Args1, R), Bounds),
    min_list(Bounds, L),
    max_list(Bounds, U),
    Res = L...U.

both(Name, Args, Res) :-
    maplist(lower(*), Args, Lower),
    Expr =.. [Name | Lower],
    eval(Expr, Res).

% General case
interval(Expr, Res, Opt),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Dir)
 => Expr =.. [ Name | Args],
    maplist(interval_(Opt), Args, Args1),
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

% Arithmetic functions for single numbers
% Define hooks for R functions etc.
eval(Expr, Res),
    eval_hook(Expr, R)
 => Res = R.

eval(X, Res)
 => Res is X.

% Comparison
int_hook(_..._ < _..._, []).
int_hook(_...A2 < B1..._, Res, _) :-
    A2 < B1,
    !,
    Res = true.

int_hook(_..._ < _..._, false, _).

int_hook(_..._ =< _..._, []).
int_hook(A1..._ =< _...B2, Res, _) :-
    A1 =< B2,
    !,
    Res = true.

int_hook(_..._ =< _..._, false, _).

int_hook(_..._ > _..._, []).
int_hook(A1..._ > _...B2, Res, _) :-
    A1 > B2,
    !,
    Res = true.

int_hook(_..._ > _..._, false, _).

int_hook(_..._ >= _..._, []).
int_hook(_...A2 >= B1..._, Res, _) :-
    A2 >= B1,
    !,
    Res = true.

int_hook(_..._ >= _..._, false, _).

int_hook(A =\= B, Res, Opt) :-
    (   interval(A < B, true, Opt)
    ;   interval(A > B, true, Opt)
    ), !,
    Res = true.

int_hook(_..._ =\= _..._, false, _).

int_hook(A =:= B, Res, Opt) :-
    interval(A =< B, true, Opt),
    interval(A >= B, true, Opt),
    !,
    Res = true.

int_hook(_..._ =:= _..._, false, _).

% Division
int_hook(_..._ / _..._, []).
int_hook(A1...A2 / B1...B2, Res, _) :-
    !,
    div(A1...A2, B1...B2, Res).

int_hook(_..._ / _, []).
int_hook(A1...A2 / B, Res, _) :-
    !,
    div(A1...A2, B...B, Res).

int_hook(_ / _..._, []).
int_hook(A / B1...B2, Res, _) :-
    !,
    div(A...A, B1...B2, Res).

int_hook(_ / _, []).
int_hook(A / B, Res, _) :-
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

% sqrt/1: "normal" behavior, returns nan for negative argument
mono(sqrt/1, [+]).

% sqrt1/1: crops negative part of interval at 0
int_hook(sqrt1(_..._), []).
int_hook(sqrt1(A...B), Res, _) :-
    strictneg(A, B),
    !,
    Res = 1.5NaN.

int_hook(sqrt1(A...B), Res, _) :-
    zeroneg(A, B),
    !,
    Res = 0.0.

int_hook(sqrt1(A...B), Res, Opt) :-
    mixed(A, B),
    !,
    interval(sqrt(0...B), Res, Opt).

int_hook(sqrt1(X), Res, Opt) :-
    interval(sqrt(X), Res, Opt).

% Power
int_hook(_..._^_, []).
int_hook(Base^Exp, Res, _) :-
    power(Base, Exp, Res).

% Even exponent with negative base
power(L...U, Exp, Res),
    negative(L, U),
    integer(Exp),
    Exp >= 0,
    Exp mod 2 =:= 0
 => eval(L^Exp, A),
    eval(U^Exp, B),
    Res = B...A.

% Even exponent with mixed base
power(L...U, Exp, Res),
    mixed(L, U),
    integer(Exp),
    Exp >= 0,
    Exp mod 2 =:= 0
 => eval(max(L^Exp, U^Exp), Upper),
    Res = 0...Upper.

% General case
power(L...U, Exp, Res),
    integer(Exp),
    Exp >= 0
 => eval(L^Exp, A),
    eval(U^Exp, B),
    Res = A...B.

% Absolute value
int_hook(abs(_..._), []).
int_hook(abs(A...B), Res, _) :-
    positive(A, B),
    !,
    Res = A...B.

int_hook(abs(A...B), Res, _) :-
    negative(A, B),
    !,
    eval(abs(A), U),
    eval(abs(B), L),
    Res = L...U.

int_hook(abs(A...B), Res, _) :-
    L = 0.0,
    U is max(abs(A), abs(B)),
    Res = L...U.

% Round interval
int_hook(round(_..._), []).
int_hook(round(A...B), Res, Opt) :-
    option(digit(Dig), Opt, 2),
    eval(floor(A, Dig), A1),
    eval(ceiling(B, Dig), B1),
    Res = A1...B1.

eval_hook(floor(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is floor(A * Mul) / Mul.

eval_hook(ceiling(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is ceiling(A * Mul) / Mul.
