:- module(rint, [interval/2, interval/3, op(150, xfx, ...), op(800, xfx, <-)]).

:- multifile r_hook/1.
:- multifile r_hook/2.
:- multifile int_hook/4.
:- multifile eval_hook/2.
:- multifile mono/2.
:- multifile interval_/3.
:- multifile interval_hook/3.

:- dynamic instantiate/2.

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- nb_setval(digits, 2).

:- consult([lib/interface, lib/core, lib/op, lib/rint_op]).

% Call R
r(Expr)
 => rolog:r_call(Expr).

% Evaluate R expression
r(Expr, Res)
 => rolog:r_eval(Expr, Res).
