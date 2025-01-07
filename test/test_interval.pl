:- module(test_interval, [test_interval/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(interval)).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

test_interval :-
    run_tests([comparison, division, sqrt, power, abs, round, sin, ci]).

:- begin_tests(comparison).

test(<) :-
    A = 1...2,
    B = 3...4,
    interval(A < B, true).

test(=<) :-
    A = 1...3,
    B = 2...4,
    interval(A =< B, true).

test(=<) :-
    A = 1...3,
    B = 1...3,
    interval(A =< B, true).

test(>) :-
    A = 3...4,
    B = 1...2,
    interval(A > B, true).

test(>=) :-
    A = 3...4,
    B = 1...2,
    interval(A >= B, true).

test(>=) :-
    A = 3...4,
    B = 3...4,
    interval(A >= B, true).

test(=\=) :-
    A = 3...4,
    B = 1...2,
    interval(A =\= B, true).

test(=\=) :-
    A = 1...2,
    B = 1...2,
    interval(A =\= B, false).

test(=:=) :-
    A = 3...4,
    B = 1...2,
    interval(A =:= B, false).

test(=:=) :-
    A = 1...2,
    B = 1...2,
    interval(A =:= B, true).

:- end_tests(comparison).

:- begin_tests(division).

test(dividend_strictpos_divisor_zeropos) :-
    A = 1...2,
    B = 0...2,
    interval(A / B, L...U),
    L is 0.5,
    U is 1.0Inf.

test(dividend_strictpos_divisor_positive) :-
    A = 1...2,
    B = 1...2,
    interval(A / B, L...U),
    L is 0.5,
    U is 2.

test(dividend_zeropos_divisor_zeropos) :-
    A = 0...2,
    B = 0...1,
    interval(A / B, L...U),
    L is 0.0,
    U is 1.0Inf.

test(dividend_zeropos_divisor_positive) :-
    A = 0...2,
    B = 1...2,
    interval(A / B, L...U),
    L is 0.0,
    U is 2.

test(dividend_mixed_divisor_zeropos) :-
    A = -1...1,
    B = 0...1,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_mixed_divisor_positive) :-
    A = -1...1,
    B = 2...4,
    interval(A / B, L...U),
    L is -0.5,
    U is 0.5.

test(dividend_zeroneg_divisor_zeropos) :-
    A = -1...0,
    B = 0...1,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is 0.0.

test(dividend_zeroneg_divisor_positive) :-
    A = -1...0,
    B = 2...3,
    interval(A / B, L...U),
    L is -0.5,
    U is 0.0.

test(dividend_strictneg_divisor_zeropos) :-
    A = -2... -1,
    B = 0...2,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is -0.5.

test(dividend_strictneg_divisor_positive) :-
    A = -2... -1,
    B = 2...4,
    interval(A / B, L...U),
    L is -1,
    U is -0.25.

test(dividend_strictpos_divisor_mixed) :-
    A = 1...2,
    B = -2...1,
    setof(Res, interval(A / B, Res), Results), 
    L1 is -1.0Inf,
    U1 is -0.5,
    L2 is 1,
    U2 is 1.0Inf,
    Results = [L1...U1, L2...U2].
    

test(dividend_zeropos_divisor_mixed) :-
    A = 0...1,
    B = -1...1,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_mixed_divisor_mixed) :-
    A = -1...1,
    B = -2...2,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_zeroneg_divisor_mixed) :-
    A = -1...0,
    B = -2...2,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_strictneg_divisor_mixed) :-
    A = -2... -1,
    B = -2...2,
    setof(Res, interval(A / B, Res), Results), 
    L1 is -1.0Inf,
    U1 is -0.5,
    L2 is 0.5,
    U2 is 1.0Inf,
    Results = [L1...U1, L2...U2].

test(dividend_strictpos_divisor_zeroneg) :-
    A = 1...2,
    B = -2...0.0,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is -0.5.

test(dividend_strictpos_divisor_negative) :-
    A = 1...2,
    B = -2... -1,
    interval(A / B, L...U),
    L is -2,
    U is -0.5.

test(dividend_zeropos_divisor_zeroneg) :-
    A = 0...1,
    B = -1...0.0,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is 0.0.

test(dividend_zeropos_divisor_negative) :-
    A = 0...1,
    B = -2... -1,
    interval(A / B, L...U),
    L is -1,
    U is 0.0.

test(dividend_mixed_divisor_zeroneg) :-
    A = -1...2,
    B = -2...0.0,
    interval(A / B, L...U),
    L is -1.0Inf,
    U is 1.0Inf.

test(dividend_mixed_divisor_negative) :-
    A = -1...2,
    B = -2... -1,
    interval(A / B, L...U),
    L is -2,
    U is 1.

test(dividend_zeroneg_divisor_zeroneg) :-
    A = -1...0,
    B = -2...0.0,
    interval(A / B, L...U),
    L is 0.0,
    U is 1.0Inf.

test(dividend_zeroneg_divisor_negative) :-
    A = -1...0,
    B = -2... -1,
    interval(A / B, L...U),
    L is 0.0,
    U is 1.

test(dividend_strictneg_divisor_zeroneg) :-
    A = -2... -1,
    B = -2...0.0,
    interval(A / B, L...U),
    L is 0.5,
    U is 1.0Inf.

test(dividend_strictneg_divisor_negative) :-
    A = -2... -1,
    B = -2... -1,
    interval(A / B, L...U),
    L is 0.5,
    U is 2.

test(dividend_interval_divisor_number) :-
    A = -2... -1,
    B is 2,
   interval(A / B, L...U),
    L is -1,
    U is -0.5.

test(dividend_number_divisor_interval) :-
    A = 2,
    B = -2... -1,
    interval(A / B, L...U),
    L is -2,
    U is -1.

test(dividend_number_divisor_number) :-
    A = 1,
    B = 2,
    interval(A / B, Res),
    Res = 0.5.

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
    interval(sqrt1(A), X),
    X = 1.5NaN.

test(sqrt6) :-
    A = 1...2,
    interval(sqrt(A), L...U),
    L > 0.99999,
    L < 1.00001,
    U > 1.41421,
    U < 1.41422.

test(sqrt7) :-
    A = 0...2,
    interval(sqrt(A), L...U),
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

:- begin_tests(power).

test(power_pos_base_even_expon) :-
    Base = 2...3,
    Exp = 2,
    interval(Base ^ Exp, L...U),
    L is 4,
    U is 9.

test(power_pos_base_odd_expon) :-
    Base = 2...3,
    Exp = 3,
    interval(Base ^ Exp, L...U),
    L is 8,
    U is 27.

test(power_neg_base_even_expon) :-
    Base = -3... -2,
    Exp = 2,
    interval(Base ^ Exp, L...U),
    L is 4,
    U is 9.

test(power_neg_base_odd_expon) :-
    Base = -3... -2,
    Exp = 3,
    interval(Base ^ Exp, L...U),
    L is -27,
    U is -8.

test(power_mixed_base_even_expon) :-
    Base = -3...2,
    Exp = 2,
    interval(Base ^ Exp, L...U),
    L is 0,
    U is 9.

test(power_mixed_base_odd_expon) :-
    Base = -3...2,
    Exp = 3,
    interval(Base ^ Exp, L...U),
    L is -27,
    U is 8.

:- end_tests(power).

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
    Dig = 2,
    interval(round(A, Dig), L...U),
    L = 2.71,
    U = 3.15.

:- end_tests(round).

:- begin_tests(sin).

test(sin_2max) :-
    L = 2,
    U = 9,
    interval(sin(L...U), Res),
    Res = -1...1.

test(sin_max) :-
    L = 1,
    U = 3,
    interval(sin(L...U), Res),
    equal(Res, 0.1411...1).

test(sin_min) :-
    L = 4,
    U = 5,
    interval(sin(L...U), Res),
    equal(Res, -1... -0.7568).

test(sin_rising) :-
    L = 1,
    U = 1.2,
    interval(sin(L...U), Res),
    equal(Res, 0.8414...0.9321).

test(sin_falling) :-
    L = 2,
    U = 2.2,
    interval(sin(L...U), Res),
    equal(Res, 0.8084...0.9093).

:- end_tests(sin).

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
    equal(A, 2.7183),
    equal(B, 7.3891).

:- end_tests(ci).

% Helper predicate to check equality
equal(Res0, Res) :-
    interval:interval(round(Res0, 4), Res).