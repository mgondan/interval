:- module(test_interval, [test_interval/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(interval)).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

test_interval :-
    run_tests([comparison, division, sqrt, power, abs, round, sin, list, no_evaluation, addition, subtraction, multiplication, exponential, unary_plus, unary_minus, max, min]).

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

test(sqrt11) :-
    interval(sqrt(9), X),
    X = 3.0.

:- end_tests(sqrt).

:- begin_tests(power).

test(power_number) :-
    Base = 2,
    Exp = 5,
    interval(Base ^ Exp, Res),
    Res is 32.

test(power_other_symbol) :-
    Base = 2,
    Exp = 5,
    interval(Base ** Exp, Res),
    Res is 32.

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

test(power_positive_base_neg_expon) :-
    Base = 2...4,
    Exp = -2,
    interval(Base ^ Exp, L...U),
    L is 0.0625,
    U is 0.25.

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

test(abs5) :-
    A = -1,
    interval(abs(A), Res),
    Res = 1.

test(abs6) :-
    A = 1,
    interval(abs(A), Res),
    Res is 1.

:- end_tests(abs).

:- begin_tests(round).

test(round1) :-
    A = 2.71828...3.14159,
    Dig = 2,
    interval(round(A, Dig), L...U),
    L = 2.71,
    U = 3.15.

test(round2) :-
    A = 0.333...0.334,
    Dig = 2,
    interval(round(A, Dig), L...U),
    L = 0.33,
    U = 0.34.

test(round3) :-
    A = 0.333...0.333,
    Dig = 2,
    interval(round(A, Dig), L...U),
    L = 0.33,
    U = 0.34.

test(round4) :-
    A = 0.333,
    Dig = 2,
    interval(round(A, Dig), L...U),
    L = 0.33,
    U = 0.34.

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

:- begin_tests(list).

test(list1) :-
    interval([], Res),
    Res = [].

test(list2) :-
    interval([1, 2+3, "age"], Res),
    Res = [1, 5, "age"].

:- end_tests(list).

:- begin_tests(no_evaluation).

test(number1) :-
    interval(5, Res),
    Res = 5.

test(number2) :-
    interval(5, L...U),
    L...U = 5...5.

test(interval) :-
    interval(1...2, Res),
    Res = 1...2.

test(logical1) :-
    interval(true, Res),
    Res = true.

test(logical2) :-
    interval(false, Res),
    Res = false.

test(atomic) :-
    interval(a, Res),
    Res = a.

test(string) :-
    interval("abc", Res),
    Res = "abc".

:- end_tests(no_evaluation).

:- begin_tests(addition).

test(addition1) :-
    interval(1.5 + 2, Res),
    Res = 3.5.

test(addition2) :-
    interval(1...2 + 1, Res),
    Res = 2...3.

test(addition3) :-
    interval(1 + 1...2, Res),
    Res = 2...3.    

test(addition4) :-
    interval(1...2 + 1...2, Res),
    Res = 2...4.

:- end_tests(addition).

:- begin_tests(subtraction).

test(subtraction1) :-
    interval(3 - 1.5, Res),
    Res = 1.5.

test(subtraction2) :-
    interval(1...2 - 1, Res),
    Res = 0...1.

test(subtraction3) :-
    interval(1 - 1...2, Res),
    Res = -1...0.    

test(addition4) :-
    interval(3...4 - 1...2, Res),
    Res = 1...3.

:- end_tests(subtraction).

:- begin_tests(multiplication).

test(multiplication1) :-
    interval(3 * 1.5, Res),
    Res = 4.5.

test(multiplication2) :-
    interval(1...2 * 3, Res),
    Res = 3...6.

test(multiplication3) :-
    interval(2 * 1...2, Res),
    Res = 2...4.    

test(multiplication4) :-
    interval(3...4 * 1...2, Res),
    Res = 3...8.

:- end_tests(multiplication).

:- begin_tests(exponential).

test(exponential1) :-
    interval(exp(2), Res),
    equal(Res, 7.3890...7.3891).

test(exponential2) :-
    interval(exp(1...2), Res),
    equal(Res, 2.7182...7.3891).

:- end_tests(exponential).

:- begin_tests(unary_plus).

test(unary_plus1) :-
    interval(+(-2), Res),
    Res is -2.

test(unary_plus2) :-
    interval(+(-1...3), Res),
    Res = -1...3.

:- end_tests(unary_plus).

:- begin_tests(unary_minus).

test(unary_minus1) :-
    interval(-(-2), Res),
    Res is 2.

test(unary_minus2) :-
    interval(-(-1...3), Res),
    Res = -3...1.

:- end_tests(unary_minus).

:- begin_tests(max).

test(max1) :-
    interval(max(1, 2), Res),
    Res is 2.

test(max2) :-
    interval(max(1...3, 1...4), Res),
    Res = 1...4.

:- end_tests(max).

:- begin_tests(min).

test(min1) :-
    interval(min(1, 2), Res),
    Res is 1.

test(min2) :-
    interval(min(1...3, 1...4), Res),
    Res = 1...3.

:- end_tests(min).

% Helper predicate to check equality
equal(Res0, Res) :-
    interval(round(Res0, 4), Res1),
    Res = Res1.

