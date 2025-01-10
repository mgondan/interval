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
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, true), Res),
    equal(Res, 12...15).

test(qbinom_uppertail) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, false), Res),
    equal(Res, 11...15).

test(pbinom_lowertail) :-
    interval(pbinom(10, 20, 0.6...0.7, true), Res),
    equal(Res, 0.0479...0.2447).

test(pbinom_uppertail) :-
    interval(pbinom(10, 20, 0.6...0.7, false), Res),
    equal(Res, 0.7553...0.9521).

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
    interval(pt(-0.5... -0.2, 5, true), Res),
    equal(Res, 0.3191...0.4247).

test(pt_lowertail_pos) :-
    interval(pt(0.5...1, 5, true), Res),
    equal(Res, 0.6808...0.8184).

test(pt_uppertail_neg) :-
    interval(pt(-0.5... -0.2, 5, false), Res),
    equal(Res, 0.5753...0.6809).

test(pt_uppertail_pos) :-
    interval(pt(0.5...1, 5, false), Res),
    equal(Res, 0.1816...0.3192).

test(pt_df_interval) :-
    interval(pt(0.5...1, 2...5, false), Res),
    equal(Res, 0.1816...0.3334).

test(qt) :-
    interval(qt(0.4...0.6, 5), Res),
    equal(Res, -0.2672...0.2672).

test(qt_df_interval) :-
    interval(qt(0.4...0.6, 2...5), Res),
    equal(Res, -0.2672...0.2887).

test(dt_neg) :-
    interval(dt(-0.5... -0.4, 5), Res),
    equal(Res, 0.3279...0.3454).

test(dt_pos) :-
    interval(dt(0.4...0.5, 5), Res),
    equal(Res, 0.3279...0.3454).

test(dt_mixed) :-
    interval(dt(-0.4... 0.5, 5), Res),
    equal(Res, 0.3279...0.3797).

test(dt_df_interval) :-
    interval(dt(-0.5... -0.4, 2...5), Res),
    equal(Res, 0.2962...0.3454).

:- end_tests(t).

:- begin_tests(chisq).

test(pchisq_lower) :-
    interval(pchisq(0.3...0.4, 5, true), Res),
    equal(Res, 0.0023...0.0047).

test(pchisq_upper) :-
    interval(pchisq(0.3...0.4, 5, false), Res),
    equal(Res, 0.9953...0.9976).

test(qchisq_lower) :-
    interval(qchisq(0.3...0.4, 5, true), Res),
    equal(Res, 2.9999...3.6555).
    
test(qchisq_upper) :-
    interval(qchisq(0.3...0.4, 5, false), Res),
    equal(Res, 5.1319...6.0644).

test(dchisq_df_lower2) :-
    interval(dchisq(0.3...0.4, 2), Res),
    equal(Res, 0.4094...0.4304).

test(dchisq_df_below_mode) :-
    interval(dchisq(1.3...1.4, 4), Res),
    equal(Res, 0.1697...0.1738).

test(dchisq_df_above_mode) :-
    interval(dchisq(1.3...1.4, 3), Res),
    equal(Res, 0.2344...0.2374).

test(dchisq_df_mixed) :-
    interval(dchisq(0.9...1.1, 3), Res),
    equal(Res, 0.2413...0.2420).

:- end_tests(chisq).

% Helper predicate to check equality
equal(Res0, Res) :-
    interval(round(Res0, 4), Res).