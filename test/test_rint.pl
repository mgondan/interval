:- module(test_rint, [test_rint/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(rint)).

test_rint :-
    run_tests([binom]).

:- begin_tests(binom).

test(dbinom1) :-
    interval(dbinom(11...12, 20...21, 0.6...0.7), L...U),
    L > 0.04118,
    L < 0.04119,
    U > 0.17970,
    U < 0.17971.

:- end_tests(binom).
