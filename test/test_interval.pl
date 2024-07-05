:- module(test_interval, [test_interval/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(interval)).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

test_interval :-
    run_tests([comparison, division, sqrt, abs, round]).

:- begin_tests(comparison).

test(<) :-
    A = 1...2,
    B = 3...4,
    interval(A < B, true).

test(=<) :-
    A = 1...3,
    B = 2...4,
    interval(A =< B, true).

:- end_tests(comparison).

:- begin_tests(division).

test(dividend_strictpos_divisor_zeropos) :-
    A = 1...2,
    B = 0...2,
    once(interval(A / B, L...U)),
    L is 0.5,
    U is 1.0Inf.

test(dividend_strictpos_divisor_positive) :-
    A = 1...2,
    B = 1...2,
    once(interval(A / B, L...U)),
    L is 0.5,
    U is 2.

test(dividend_zeropos_divisor_zeropos) :-
    A = 0...2,
    B = 0...1,
    once(interval(A / B, L...U)),
    L is 0.0,
    U is 1.0Inf.

test(dividend_zeropos_divisor_positive) :-
    A = 0...2,
    B = 1...2,
    once(interval(A / B, L...U)),
    L is 0.0,
    U is 2.

test(dividend_mixed_divisor_zeropos) :-
    A = -1...1,
    B = 0...1,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_mixed_divisor_positive) :-
    A = -1...1,
    B = 2...4,
    once(interval(A / B, L...U)),
    L is -0.5,
    U is 0.5.

test(dividend_zeroneg_divisor_zeropos) :-
    A = -1...0,
    B = 0...1,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is 0.0.

test(dividend_zeroneg_divisor_positive) :-
    A = -1...0,
    B = 2...3,
    once(interval(A / B, L...U)),
    L is -0.5,
    U is 0.0.

test(dividend_strictneg_divisor_zeropos) :-
    A = ...(-2,-1),
    B = 0...2,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is -0.5.

test(dividend_strictneg_divisor_positive) :-
    A = ...(-2,-1),
    B = 2...4,
    once(interval(A / B, L...U)),
    L is -1,
    U is -0.25.

test(dividend_strictpos_divisor_mixed) :-
    A = 1...2,
    B = -2...1,
    once(interval(A / B, L...U)), 
    L is -1.0Inf,
    U is -0.5.

test(dividend_zeropos_divisor_mixed) :-
    A = 0...1,
    B = -1...1,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_mixed_divisor_mixed) :-
    A = -1...1,
    B = -2...2,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_zeroneg_divisor_mixed) :-
    A = -1...0,
    B = -2...2,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_strictneg_divisor_mixed) :-
    A = ...(-2,-1),
    B = -2...2,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is -0.5.

test(dividend_strictpos_divisor_zeroneg) :-
    A = 1...2,
    B = -2...0.0,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is -0.5.

test(dividend_strictpos_divisor_negative) :-
    A = 1...2,
    B = ...(-2,-1),
    once(interval(A / B, L...U)),
    L is -2,
    U is -0.5.

test(dividend_zeropos_divisor_zeroneg) :-
    A = 0...1,
    B = -1...0.0,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is 0.0.

test(dividend_zeropos_divisor_negative) :-
    A = 0...1,
    B = ...(-2,-1),
    once(interval(A / B, L...U)),
    L is -1,
    U is 0.0.

test(dividend_mixed_divisor_zeroneg) :-
    A = -1...2,
    B = -2...0.0,
    once(interval(A / B, L...U)),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_mixed_divisor_negative) :-
    A = -1...2,
    B = ...(-2,-1),
    once(interval(A / B, L...U)),
    L is -2,
    U is 1.

test(dividend_zeroneg_divisor_zeroneg) :-
    A = -1...0,
    B = -2...0.0,
    once(interval(A / B, L...U)),
    L is 0.0,
    U is 1.0Inf.

test(dividend_zeroneg_divisor_negative) :-
    A = -1...0,
    B = ...(-2,-1),
    once(interval(A / B, L...U)),
    L is 0.0,
    U is 1.

test(dividend_strictneg_divisor_zeroneg) :-
    A = ...(-2,-1),
    B = -2...0.0,
    once(interval(A / B, L...U)),
    L is 0.5,
    U is 1.0Inf.

test(dividend_strictneg_divisor_negative) :-
    A = ...(-2,-1),
    B = ...(-2,-1),
    once(interval(A / B, L...U)),
    L is 0.5,
    U is 2.

test(dividend_interval_divisor_number) :-
    A = ...(-2,-1),
    B is 2,
    once(interval(A / B, L...U)),
    L is -1,
    U is -0.5.

test(dividend_number_divisor_interval) :-
    A = 2,
    B = ...(-2,-1),
    once(interval(A / B, L...U)),
    L is -2,
    U is -1.

test(dividend_number_divisor_number) :-
    A = 1,
    B = 2,
    interval(A / B, Res),
    Res is 0.5.

:- end_tests(division).

:- begin_tests(sqrt).

test(sqrt1) :-
    A = 1...2,
    interval(sqrt(A), L...U),
    L > 0.99999,
    L < 1.00001,
    U > 1.41421,
    U < 1.41422.

test(sqrt2) :-
    A = 0...2,
    interval(sqrt(A), L...U),
    L = 0.0,
    U > 1.41421,
    U < 1.41422.

test(sqrt3) :-
    A = -1...2,
    interval(sqrt(A), L...U),
    L = 1.5NaN,
    U > 1.41421,
    U < 1.41422.

test(sqrt4) :-
    A = -2...0,
    interval(sqrt(A), L...U),
    L = 1.5NaN,
    U = 0.0.

test(sqrt5) :-
    A = -2... -1,
    interval(sqrt(A), X),
    X = 1.5NaN...1.5NaN.

test(sqrt6) :-
    A = 1...2,
    interval(sqrt1(A), L...U),
    L > 0.99999,
    L < 1.00001,
    U > 1.41421,
    U < 1.41422.

test(sqrt7) :-
    A = 0...2,
    interval(sqrt1(A), L...U),
    L = 0.0,
    U > 1.41421,
    U < 1.41422.

test(sqrt8) :-
    A = -1...2,
    interval(sqrt1(A), L...U),
    L = 0.0,
    U > 1.41421,
    U < 1.41422.

test(sqrt9) :-
    A = -2...0,
    interval(sqrt1(A), X),
    X = 0.0.

test(sqrt10) :-
    A = -2... -1,
    interval(sqrt1(A), X),
    X = 1.5NaN.

:- end_tests(sqrt).

:- begin_tests(abs).

test(abs1) :-
    A = 1...2,
    interval(abs(A), L...U),
    L > 0.99999,
    L < 1.00001,
    U > 1.99999,
    U < 2.00001.

test(abs2) :-
    A = -1...2,
    interval(abs(A), L...U),
    L = 0.0,
    U > 1.99999,
    U < 2.00001.

test(abs3) :-
    A = -2...1,
    interval(abs(A), L...U),
    L = 0.0,
    U > 1.99999,
    U < 2.00001.

test(abs4) :-
    A = -2... -1,
    interval(abs(A), L...U),
    L > 0.99999,
    L < 1.00001,
    U > 1.99999,
    U < 2.00001.

:- end_tests(abs).

:- begin_tests(round).

test(round1) :-
    A = 2.71828...3.14159,
    interval(round(A), L...U),
    L = 2.71,
    U = 3.15.

:- end_tests(round).

