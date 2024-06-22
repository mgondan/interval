:- module(test_interval, [test_interval/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(interval)).

test_interval :-
    run_tests([interval]).

:- begin_tests(interval).

test((<)) :-
    A = 1...2,
    B = 3...4,
    interval(A < B, false).

:- end_tests(interval).

