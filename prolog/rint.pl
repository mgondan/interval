:- module(rint, [interval/2, interval/3, op(150, xfx, ...), op(800, xfx, <-)]).

:- multifile r_hook/1.
:- multifile r_hook/2.
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

:- consult([lib/interface, lib/core, lib/op, lib/rint_op]).

/** <module> Use intervals in R functions.

This module expands the 'interval' module with R functions.
For general information on the use of interval/2, please refer to that module. 
 */

% Binomial distribution
% - Cumulated density lower-tail: interval(pbinom(X, N, P, true), Res)
% - Cumulated density upper-tail: interval(pbinom(X, N, P, false), Res)
% - Quantile: interval(qbinom(Alpha, N, P, true), Res)
% - Density: interval(dbinom(X, N, P), Res)
%
% Normal distribution
% - Cumulated density: interval(pnorm(X, Mu, Sigma), Res)
% - Quantile: interval(qnorm(P, Mu, Sigma), Res)
% - Density: interval(dnorm(X, Mu, Sigma), Res)
%
% T distribution
% - Cumulated density lower-tail: interval(pt(X, Df, true), Res)
% - Cumulated density upper-tail: interval(pt(X, Df, false), Res)
% - Quantile: interval(qt(P, Df), Res)
% - Density: interval(dt(X, Df), Res)
