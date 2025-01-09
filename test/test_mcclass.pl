:- module(test_mcclass, [test_mcclass/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(mcclass)).

test_mcclass :-
    run_tests([fractions, number_digit, omit, multiply, available, equality, plus, ci]).

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

test(tstat_atomic) :-
    A = 1,
    B = 3,
    interval(tstat(A / B), Res),
    Res is 0.33.

test(tstat_interval) :-
    A = 1...5,
    B = 3...6,
    interval(tstat(A / B), L...U),
    L is 0.16,
    U is 1.67.

test(hdrs_atomic) :-
    A = 1,
    B = 3,
    interval(hdrs(A / B), Res),
    Res is 0.3.

test(hdrs_interval) :-
    A = 1...5,
    B = 3...6,
    interval(hdrs(A / B), L...U),
    L is 0.1,
    U is 1.7.

test(chi2ratio_atomic) :-
    A = 1,
    B = 3,
    interval(chi2ratio(A / B), Res),
    Res is 0.33.

test(chi2ratio_interval) :-
    A = 1...5,
    B = 3...6,
    interval(chi2ratio(A / B), L...U),
    L is 0.16,
    U is 1.67.

test(pval_atomic) :-
    A = 1,
    B = 3,
    interval(pval(A / B), Res),
    Res is 0.333.

test(pval_interval) :-
    A = 1...5,
    B = 3...6,
    interval(pval(A / B), L...U),
    L is 0.166,
    U is 1.667.

:- end_tests(number_digit).

:- begin_tests(omit).
test(omit_right_atomic) :-
    A = 5,
    B = 4,
    interval(omit_right(A - B), Res),
    Res = A.

test(omit_right_interval) :-
    A = 11...12,
    B = 20...21,
    interval(omit_right(A - B), Res),
    Res = A.

test(omit_left_atomic) :-
    A = 5,
    B = 4,
    interval(omit_left(A - B), Res),
    Res = B.

test(omit_left_interval) :-
    A = 11...12,
    B = 20...21,
    interval(omit_left(A - B), Res),
    Res = B.

:- end_tests(omit).

:- begin_tests(multiply).

test(dot_atomic) :-
    A = 2,
    B = 3,
    interval(dot(A, B), Res),
    Res is 6.

test(dot_interval) :-
    A = 2...3,
    B = 3...4,
    interval(dot(A, B), L...U),
    L is 6,
    U is 12.

:- end_tests(multiply).

:- begin_tests(available).

test(available_atomic1) :-
    A = 1,
    B = 2,
    interval(available(A + B), Res),
    Res = true.

test(available_atomic2) :-
    A = 0,
    B = 0,
    interval(available(A / B), Res),
    Res = false.

test(available_interval) :-
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

test(equality_interval2) :-
    A = 1...2,
    B = 3...4,
    interval(A =@= B, false).

:- end_tests(equality).

:- begin_tests(plus).

test(plus1) :-
    A = 1,
    B = 2,
    interval(plus(A, B), Res),
    Res is 3.

test(plus2) :-
    A = 1...2,
    B = 2...3,
    interval(plus(A, B), Res),
    Res = 3...5.

:- end_tests(plus).

:- begin_tests(ci).

test(ciplus1) :-
    interval(ci(1, 2) + 3, ci(4, 5)).

test(ciplus2) :-
    interval(3 + ci(1, 2), ci(4, 5)).

test(ciminus) :-
    interval(ci(1, 2) - 3, ci(-2, -1)).

test(cimult) :-
    interval(ci(1, 2) * 3, ci(3, 6)).

test(cidiv) :-
    interval(ci(2, 4) / 8, ci(0.25, 0.5)).

test(ciexp) :-
    interval(exp(ci(1, 2)), ci(A, B)),
    test_mcclass:equal(A, 2.7183),
    test_mcclass:equal(B, 7.3891).

:- end_tests(ci).

% Helper predicate to check equality
equal(Res0, Res) :-
    mcint:interval(round(Res0, 4), Res).