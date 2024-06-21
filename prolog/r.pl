:- module(r, [r_initialize/0, r/1, r/2, r_source/1]).

:- use_module(library(rologp)).

:- dynamic r_initialized/0.

% Initialize R, load some code into the base environment.
r_initialize,
    r_initialized
 => true.

r_initialize
 => r_source(r),
    assert(r_initialized).

% Call R
r(Expr)
 => r_call(Expr).

% Evaluate R expression
r(Expr, Res)
 => r_eval(Expr, Res).

r_source(File)
 => format(string(String), "~w.R", [File]),
    r(source(String)).

