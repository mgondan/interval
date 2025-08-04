:- module(rint, [interval/2, interval/3, op(150, xfx, ...), op(800, xfx, <-)]).

/** <module> Interval arithmetic in R 

Used via the R package 'rolog'.

The only difference to the 'rint' prologe module is that the moudle 'r' is not loaded, 
because 'rolog' is already initialized on the R side.
*/

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- nb_setval(digits, 2).

:- consult(['lib/cleaning', 'lib/interface', 'lib/rint_op','lib/op', 'lib/eval_r', 'lib/utility']).

% Call R
r(Expr)
 => rolog:r_call(Expr).

% Evaluate R expression
r(Expr, Res)
 => rolog:r_eval(Expr, Res).
