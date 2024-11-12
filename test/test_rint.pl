:- module(test_rint, [test_rint/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(rint)).

test_rint :-
    run_tests([binom, normal, t]).

:- begin_tests(binom).

test(dbinom1) :-
    interval(dbinom(11...12, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0411...0.1798).

% left to X / N
test(dbinom2) :- 
    interval(dbinom(10...11, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0176...0.1598).

% right to X / N
test(dbinom3) :- 
    interval(dbinom(15...16, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0349...0.1879).

test(qbinom_lowertail) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, atomic(true)), Res),
    equal(Res, 12...15).

test(qbinom_uppertail) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, atomic(false)), Res),
    equal(Res, 11...15).

test(pbinom_lowertail) :-
    interval(pbinom(10...11, 20...21, 0.6...0.7, atomic(true)), Res),
    equal(Res, 0.0263...0.4045).

test(pbinom_uppertail) :-
    interval(pbinom(10...11, 20...21, 0.6...0.7, atomic(false)), Res),
    equal(Res, 0.5955...0.9737).

:- end_tests(binom).

:- begin_tests(normal).

test(pnorm) :-
    interval(pnorm(90...91, 100...101, 10...11), Res),
    equal(Res, 0.1356...0.2067).

test(qnorm) :-
    interval(qnorm(0.6...0.7, 100...101, 10...11), Res),
    equal(Res, 102.5334...106.7685).

test(dnorm_z_neg) :-
    interval(dnorm(90...91, 100...101, 10...11), Res),
    equal(Res, 0.0198...0.0286).

test(dnorm_z_pos) :-
    interval(dnorm(110...111, 100...101, 10...11), Res),
    equal(Res, 0.0198...0.0286).

test(dnorm_z_mixed) :-
    interval(dnorm(99...102, 100...101, 10...11), Res),
    equal(Res, 0.0355...0.0399).

:- end_tests(normal).

:- begin_tests(t).

test(pt_lowertail_neg) :-
    interval(pt(-0.5... -0.2, atomic(5), atomic(true)), Res),
    equal(Res, 0.3191...0.4247).

test(pt_lowertail_pos) :-
    interval(pt(0.5...1, atomic(5), atomic(true)), Res),
    equal(Res, 0.6808...0.8184).

test(pt_uppertail_neg) :-
    interval(pt(-0.5... -0.2, atomic(5), atomic(false)), Res),
    equal(Res, 0.5753...0.6809).

test(pt_uppertail_pos) :-
    interval(pt(0.5...1, atomic(5), atomic(false)), Res),
    equal(Res, 0.1816...0.3192).

test(qt) :-
    interval(qt(0.4...0.6, atomic(5)), Res),
    equal(Res, -0.2672...0.2672).

test(dt_neg) :-
    interval(dt(-0.5... -0.4, atomic(5)), Res),
    equal(Res, 0.3279...0.3454).

test(dt_pos) :-
    interval(dt(0.4...0.5, atomic(5)), Res),
    equal(Res, 0.3279...0.3454).

test(dt_mixed) :-
    interval(dt(-0.4... 0.5, atomic(5)), Res),
    equal(Res, 0.3279...0.3797).

:- end_tests(t).

% Helper predicate to check equality
equal(Res0, Res) :-
    interval:interval(round(Res0, atomic(4)), Res).