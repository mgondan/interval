:- module(test_mcclass, [test_mcclass/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(mcclass)).

test_mcclass :-
    run_tests([fractions, number_digit, omit, multiply, available, equality]).

:- begin_tests(fractions).

test(frac) :-
    A = 1...2,
    B = 2...4,
    interval(frac(A, B), L...U),
    L is 0.25,
    U is 1.

test(dfrac) :-
    A = 1...2,
    B = 2...4,
    interval(dfrac(A, B), L...U),
    L is 0.25,
    U is 1.

:- end_tests(fractions).

:- begin_tests(number_digit).

test(tstat) :-
    A = 1...5,
    B = 3...6,
    interval(tstat(A / B), L...U),
    L is 0.16,
    U is 1.67.

test(hdrs) :-
    A = 1...5,
    B = 3...6,
    interval(hdrs(A / B), L...U),
    L is 0.1,
    U is 1.7.

test(chi2ratio) :-
    A = 1...5,
    B = 3...6,
    interval(chi2ratio(A / B), L...U),
    L is 0.16,
    U is 1.67.

test(pval) :-
    A = 1...5,
    B = 3...6,
    interval(pval(A / B), L...U),
    L is 0.166,
    U is 1.667.

:- end_tests(number_digit).

:- begin_tests(omit).

test(omit_left) :-
    A = 11...12,
    B = 20...21,
    C = 10...11,
    interval(omit_left(A - (B - C)), Res),
    Res = 9...11.

test(omit_right) :-
    A = 11...12,
    B = 20...21,
    C = 10...11,
    interval(omit_right((B - C), A), Res),
    Res = 9...11.

:- end_tests(omit).

:- begin_tests(multiply).

test(dot) :-
    A = 2...3,
    B = 3...4,
    interval(dot(A, B), L...U),
    L is 6,
    U is 12.

:- end_tests(multiply).

:- begin_tests(available).

test(available_int) :-
    A = 1...3,
    B = 2...5,
    interval(available(A + B), Res),
    Res = true.

test(available_float) :-
    A = 1.1...3.1,
    B = 2.1...5.1,
    interval(available(A + B), Res),
    Res = true.

test(not_available_nan) :-
    interval(available(0 / 0), Res),
    Res = false.

:- end_tests(available).

:- begin_tests(equality).

test(equality_atomic1) :-
    interval(5 =@= 5, true).

test(equality_atomic2) :-
    interval(5 =@= 4, false).

test(equality_interval1) :-
    A = 1...2,
    B = 2...3,
    interval(A =@= B, true).

test(equality_interval1) :-
    A = 1...2,
    B = 3...4,
    interval(A =@= B, false).

:- end_tests(equality).