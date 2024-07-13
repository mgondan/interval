:- module(test_mcclass, [test_mcclass/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(mcclass)).

test_mcclass :-
    run_tests([fractions, number_digit, omit, multiply, available, confidence_intervals, equality]).

:- begin_tests(fractions).

test(frac) :-
    interval(frac(1...2, 2...4), L...U),
    L is 0.25,
    U is 1.

test(dfrac) :-
    interval(dfrac(1...2, 2...4), L...U),
    L is 0.25,
    U is 1.

:- end_tests(fractions).

:- begin_tests(number_digit).

% ToDo: adjust expected results after implementation of option evaluation 
test(tstat) :-
    interval(tstat(1...5 / 3...6), L...U),
    L > 0.1666,
    L < 0.1667,
    U > 1.6666,
    U < 1.6667.

test(hdrs) :-
    interval(hdrs(1...5 / 3...6), L...U),
    L > 0.1666,
    L < 0.1667,
    U > 1.6666,
    U < 1.6667.

test(chi2ratio) :-
    interval(chi2ratio(1...5 / 3...6), L...U),
    L > 0.1666,
    L < 0.1667,
    U > 1.6666,
    U < 1.6667.

test(pval) :-
    interval(pval(1...5 / 3...6), L...U),
    L > 0.1666,
    L < 0.1667,
    U > 1.6666,
    U < 1.6667.

:- end_tests(number_digit).

:- begin_tests(omit).

test(omit_left) :-
    interval(omit_left(11...12 - 20...21), L...U),
    L > 19.9999,
    L < 20.0001,
    U > 20.9999,
    U < 21.0001.

test(omit_right) :-
    interval(omit_right(11...12 - 20...21), L...U),
    L > 10.9999,
    L < 11.0001,
    U > 11.9999,
    U < 12.0001.

:- end_tests(omit).

:- begin_tests(multiply).

test(dot) :-
    interval(dot(2...3, 3...4), L...U),
    L is 6,
    U is 12.

:- end_tests(multiply).

:- begin_tests(available).

test(available_int) :-
    interval(available(1...3 + 2...5), Res),
    Res = true.

test(available_float) :-
    interval(available(1.1...3.1 + 2.1...5.1), Res),
    Res = true.

test(not_available_nan) :-
    interval(available(0 / 0), Res),
    Res = false.

:- end_tests(available).

:- begin_tests(confidence_intervals).

test(pm) :-
    A = 1...2,
    B = 3...4,
    interval(pm(A, B), ci(LA, UA)),
    LA = -3... -1,
    UA = 4...6.

test(ninfpos) :-
    A = 1...2,
    interval(ninfpos(A), ci(LA, UA)),
    LA = -1.0Inf,
    UA = 1...2.

test(ninfpos_upper) :-
    A = 1...2,
    interval(ninfpos(A), Res, [ci(upper)]),
    Res = 1...2.

test(ninfpos_lower) :-
    A = 1...2,
    interval(ninfpos(A), Res, [ci(lower)]),
    Res = -1.0Inf.

test(neginf) :-
    A = 1...2,
    interval(neginf(A), ci(LA, UA)),
    LA = 1...2,
    UA = 1.0Inf.

test(neginf_upper) :-
    A = 1...2,
    interval(neginf(A), Res, [ci(upper)]),
    Res = 1.0Inf.

test(neginf_lower) :-
    A = 1...2,
    interval(neginf(A), Res, [ci(lower)]),
    Res = 1...2.

test(ci) :-
    LA = 1...2,
    UA = 3...4, 
    interval(ci(LA, UA), ci(LA, UA)).
    
test(ci_upper) :-
    LA = 1...2,
    UA = 3...4, 
    interval(ci(LA, UA), Res, [ci(upper)]),
    Res = 3...4.

test(ci_lower) :-
    LA = 1...2,
    UA = 3...4, 
    interval(ci(LA, UA), Res, [ci(lower)]),
    Res = 1...2.

:- end_tests(confidence_intervals).

:- begin_tests(equality).

test(overlapping_true) :-
    A = 1...2,
    B = 2...3,
    interval(A =@= B, Res),
    Res = true.

test(overlapping_false) :-
    A = 1...2,
    B = 3...4,
    interval(A =@= B, Res),
    Res = false.

:- end_tests(equality).