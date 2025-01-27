:- module(mcint, [interval/2, interval/3, op(150, xfx, ...)]).

:- multifile r_hook/1.
:- multifile r_hook/2.
:- multifile int_hook/4.
:- multifile eval_hook/2.
:- multifile mono/2.
:- multifile interval_/3.
:- multifile instantiate/2.

:- dynamic r_topic/2.

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

:- nb_setval(digits, 2).

:- consult([lib/interface, lib/ci, lib/core, lib/op, lib/mcclass_op, lib/rint_op]).