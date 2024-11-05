:- module(test_rint, [test_rint/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(rint)).

test_rint :-
    run_tests([binom, normal]).

:- begin_tests(binom).

test(dbinom1) :-
    interval(dbinom(11...12, 20...21, 0.6...0.7), L...U),
    L > 0.04118,
    L < 0.04119,
    U > 0.17970,
    U < 0.17971.

% left to X / N
test(dbinom2) :- 
    interval(dbinom(10...11, 20...21, 0.6...0.7), L...U),
    L > 0.01764,
    L < 0.01765,
    U > 0.15973,
    U < 0.15974.

% right to X / N
test(dbinom3) :- 
    interval(dbinom(15...16, 20...21, 0.6...0.7), L...U),
    L > 0.03499,
    L < 0.03500,
    U > 0.18780,
    U < 0.18781.

test(qbinom_lowertail) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, atomic(true)), L...U),
    L > 11.99999,
    L < 12.00001,
    U > 14.99999,
    U < 15.00001.

test(qbinom_uppertail) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, atomic(false)), L...U),
    L > 10.99999,
    L < 11.00001,
    U > 14.99999,
    U < 15.00001.

test(pbinom_lowertail) :-
    interval(pbinom(10...11, 20...21, 0.6...0.7, atomic(true)), L...U),
    L > 0.02638,
    L < 0.02639,
    U > 0.40440,
    U < 0.40441.

test(pbinom_uppertail) :-
    interval(pbinom(10...11, 20...21, 0.6...0.7, atomic(false)), L...U),
    L > 0.59559,
    L < 0.59560,
    U > 0.97361,
    U < 0.97362.

:- end_tests(binom).

:- begin_tests(normal).

test(pnorm) :-
    interval(pnorm(90...91, 100...101, 10...11), L...U),
    L > 0.13566,
    L < 0.13567,
    U > 0.20662,
    U < 0.20663.

test(qnorm) :-
    interval(qnorm(0.6...0.7, 100...101, 10...11), L...U),
    L > 102.53347,
    L < 102.53348,
    U > 106.76840,
    U < 106.76841.

test(dnorm_z_neg) :-
    interval(dnorm(90...91, 100...101, 10...11), L...U),
    L > 0.01980,
    L < 0.01981,
    U > 0.02854,
    U < 0.02855.

test(dnorm_z_pos) :-
    interval(dnorm(110...111, 100...101, 10...11), L...U),
    L > 0.01980,
    L < 0.01981,
    U > 0.02854,
    U < 0.02855.

test(dnorm_z_mixed) :-
    interval(dnorm(99...102, 100...101, 10...11), L...U),
    L > 0.03554,
    L < 0.03555,
    U > 0.03989,
    U < 0.03990.

:- end_tests(normal).