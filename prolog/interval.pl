:- module(interval, [interval/2, interval/3, op(150, xfx, ...)]).

:- multifile int_hook/4.
:- multifile eval_hook/2.
:- multifile mono/2.
:- multifile interval_/3.
:- multifile instantiate/2.
:- multifile interval_hook/3.

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- nb_setval(digits, 2).

:- consult(['../inst/prolog/lib/interface', '../inst/prolog/lib/core', '../inst/prolog/lib/op']).

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