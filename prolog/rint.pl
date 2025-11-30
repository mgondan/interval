:- module(rint, [interval/2, interval/3, op(150, xfx, ...), op(800, xfx, <-)]).

/** <module> Use intervals in R functions.

This module expands the 'interval' module with R functions. 
For general information on the use of interval/2 and interval/3, please refer to that module. 
For better compatibility with R, the standard built-in arithmetic evaluation is used 
instead of the library clpBNR as in the module 'interval'. 
*/

% Supported functions:
%   Assignment 
%       - '<-'
%       
%   Binomial distribution
%       - Cumulated density with default lower tail: interval(pbinom(X, N, P), Res)
%       - Cumulated density with explicit tail argument: interval(pbinom(X, N, P, true), Res)
%       - Cumulated density with explicit tail and log.p argument: interval(pbinom(X, N, P, false, true), Res)
%       - Quantile with default lower tail: interval(qbinom(Alpha, N, P), Res)
%       - Quantile with explicit tail argument: interval(qbinom(Alpha, N, P, true), Res)
%       - Density: interval(dbinom(X, N, P), Res)
%
%   Normal distribution
%       - Cumulated density with defaults: interval(pnorm(X), Res)
%       - Cumulated density with default lower tail: interval(pnorm(X, Mu, Sd), Res)
%       - Cumulated density with explicit tail argument: interval(pnorm(X, Mu, Sd, false), Res)
%       - Quantile with defaults: interval(qnorm(P), Res)
%       - Quantile with default lower tail: interval(qnorm(P, Mu, Sigma), Res)
%       - Quantile with explicit tail argument: interval(qnorm(P, Mu, Sigma, true), Res)
%       - Density with defaults: interval(dnorm(X), Res)
%       - Density: interval(dnorm(X, Mu, Sigma), Res)
%
%   T distribution
%       - Cumulated density with default lower tail: interval(pt(X, Df), Res)
%       - Cumulated density explicit tail argument: interval(pt(X, Df, false), Res)
%       - Quantile with default lower tail: interval(qt(P, Df), Res)
%       - Quantile with explicit tail argument: interval(qt(P, Df, true), Res)
%       - Density: interval(dt(X, Df), Res)
%
%   Chi-square distribution
%       - Cumulated density with default lower tail: interval(pchisq(X, Df), Res)
%       - Cumulated density explicit tail argument: interval(pchisq(X, Df, false), Res)
%       - Quantile with default lower tail: interval(qchisq(P, Df), Res)
%       - Quantile with explicit tail argument: interval(qchisq(P, Df, true), Res)
%       - Density: interval(dchisq(X, Df), Res)

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- nb_setval(digits, 2).

:- consult(['../inst/prolog/lib/interface', 
            '../inst/prolog/lib/rint_op', 
            '../inst/prolog/lib/op', 
            '../inst/prolog/lib/eval_r',
            'r', 
            '../inst/prolog/lib/utility']).
