:- module(test_interval, [test_interval/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(interval)).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

test_interval :-
    run_tests([comparison]).

:- begin_tests(comparison).

test(<) :-
    A = 1...2,
    B = 3...4,
    interval(A < B, true).

test(=<) :-
    A = 1...3,
    B = 2...4,
    interval(A =< B, true).

test(sqrt1) :-
    A = 1...2,
    interval(sqrt(A), L...U),
    L > 0.99999,
    L < 1.00001,
    U > 1.41429,
    U < 1.41430.

test(sqrt2) :-
    A = 0...2,
    interval(sqrt(A), L...U),
    L = 0.0,
    U > 1.41429,
    U < 1.41430.

test(sqrt3) :-
    A = -1...2,
    interval(sqrt(A), L...U),
    L = 1.5NaN,
    U > 1.41429,
    U < 1.41430.

test(sqrt4) :-
    A = -2...0,
    interval(sqrt(A), L...U),
    L = 0.0,
    U = 0.0.

test(sqrt5) :-
    A = -2... -1,
    interval(sqrt(A), X),
    X = 1.5NaN.

test(sqrt6) :-
    A = 1...2,
    interval(sqrt1(A), L...U),
    L > 0.99999,
    L < 1.00001,
    U > 1.41429,
    U < 1.41430.

test(sqrt7) :-
    A = 0...2,
    interval(sqrt1(A), L...U),
    L = 0.0,
    U > 1.41429,
    U < 1.41430.

test(sqrt8) :-
    A = -1...2,
    interval(sqrt1(A), L...U),
    L = 0.0,
    U > 1.41429,
    U < 1.41430.

test(sqrt9) :-
    A = -2...0,
    interval(sqrt1(A), L...U),
    L = 0.0,
    U = 0.0.

test(sqrt10) :-
    A = -2... -1,
    interval(sqrt1(A), X),
    X = 0.0.

:- end_tests(comparison).

