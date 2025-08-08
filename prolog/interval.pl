:- module(interval, [interval/2, interval/3, op(150, xfx, ...)]).

/** <module> Perform arithmetic operations with intervals.

An interval is represented as L...U, where L stands for the lower bound and 
U for the upper bound. If the upper bound is a negative number, it has to be written with 
an additional space, e.g., -3... -2, or in the infix notation, ...(-3, -2).  
The choice of using consult and not the module system is motivated by need for more flexibility.
*/

%%  interval(+A, ?Res) is nondet
%
%   True if A evalutes to Res. Atoms, numbers, and strings are not evaluated. 
%   Supported operations: 
%     - Comparison: '>', '<', '>=', '=<', '=\=', '=:='
%     - Basic arithemtic: addition '+', subtraction '-', division '/', multiplication '*'
%     - Square root for positive interval: 'interval(sqrt(A), Res)'
%     - Square root for negative or mixed interval: 'interval(sqrt1(A), Res)'
%     - Power: 'interval(A^atomic(N), Res)' or 'interval(A**atomic(N), Res)' with N being a natural number
%     - Exponential: 'interval(exp(A)), Res)' 
%     - Absolute value: 'interval(abs(A), Res)'
%     - Sine: 'interval(sin(A), Res)'
%     - Unary plus: 'interval(+(A), Res)'
%     - Unary minus: 'interval(-(A), Res)'
%     - Max: 'interval(max(A, B), Res)'
%     - Min: 'interval(min(A, B), Res)'
%     - Rounding: 'interval(round(1.356...1.634, atomic(2)), Res)' 
%   
%   @arg A is the expression to be evaluted.
%   @arg Res is the result. This should always be unbound. For checking results, use 'interval(1...2 + 1, Res), Res = 2...3' instead of 'interval(1...2 + 1, 2...3)'

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- nb_setval(digits, 2).

:- consult(['../inst/prolog/lib/interface', 
            '../inst/prolog/lib/op', 
            '../inst/prolog/lib/eval',
            '../inst/prolog/lib/utility']).