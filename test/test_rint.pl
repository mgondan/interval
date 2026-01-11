:- module(test_rint, [test_rint/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(rint)).

test_rint :-
    run_tests([r, assignment, colon, binom, normal, t, chisq]).

:- begin_tests(r).

test(r1) :-
    A is 1,
    interval(<-(x, A), _),
    interval(r(x), Res),
    Res = A.

test(r2) :-
    interval(r(1 + 1) + 2...3, Res),
    Res = 4...5.

test(r3) :-
    interval(dbinom(1...2, 10, 0.7), L1...U1),
    interval(r(dbinom(1...2, 10, 0.7)), L2...U2),
    L1 =:= L2,
    U1 =:= U2.

:- end_tests(r).

:- begin_tests(assignment).

test(assign1) :-
    A = 1,
    interval(a <- A, _Res),
    interval(r(a), Res),
    Res =:= A.

test(assign2) :-
    A = 1...2,
    interval(a <- A, _Res),
    interval(r(a), Res),
    Res = A.

:- end_tests(assignment).

:- begin_tests(colon).

test(colon1) :-
    A = 1:10,
    interval(A, A).

:- end_tests(colon).

:- begin_tests(binom).

test(dbinom1a) :-
    interval(dbinom(11, 20, 0.6), Res),
    equal(Res, 0.1597...0.1598).

test(dbinom1b) :-
    interval(dbinom(11, 20, 0.6, false), Res),
    equal(Res, 0.1597...0.1598).

test(dbinom1c) :-
    interval(dbinom(11, 20, 0.6, true), Res),
    equal(Res, -1.8343... -1.8342).

test(dbinom2a) :-
    interval(dbinom(11...12, 20, 0.6...0.7), Res),
    equal(Res, 0.0653...0.1798).

test(dbinom2b) :-
    interval(dbinom(11...12, 20, 0.6...0.7, false), Res),
    equal(Res, 0.0653...0.1798).

test(dbinom2c) :-
    interval(dbinom(11...12, 20, 0.6...0.7, true), Res),
    equal(Res, -2.7277... -1.7164).

test(dbinom3a) :-
    interval(dbinom(11...12, 20, 0.6), Res),
    equal(Res, 0.1597...0.1798).

test(dbinom3b) :-
    interval(dbinom(11...12, 20, 0.6, false), Res),
    equal(Res, 0.1597...0.1798).

test(dbinom3c) :-
    interval(dbinom(11...12, 20, 0.6, true), Res),
    equal(Res, -1.8343... -1.7164).

test(dbinom4a) :-
    interval(dbinom(11...12, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0411...0.1798).

test(dbinom4b) :-
    interval(dbinom(11...12, 20...21, 0.6...0.7, false), Res),
    equal(Res, 0.0411...0.1798).

test(dbinom4c) :-
    interval(dbinom(11...12, 20...21, 0.6...0.7, true), Res),
    equal(Res, -3.1898... -1.7164).

% left to X / N
test(dbinom5a) :- 
    interval(dbinom(10...11, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0176...0.1598).

test(dbinom5b) :- 
    interval(dbinom(10...11, 20...21, 0.6...0.7, false), Res),
    equal(Res, 0.0176...0.1598).

test(dbinom5c) :- 
    interval(dbinom(10...11, 20...21, 0.6...0.7, true), Res),
    equal(Res, -4.0371... -1.8342).

% right to X / N
test(dbinom6a) :- 
    interval(dbinom(15...16, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0349...0.1879).

test(dbinom6b) :- 
    interval(dbinom(15...16, 20...21, 0.6...0.7, false), Res),
    equal(Res, 0.0349...0.1879).

test(dbinom6c) :- 
    interval(dbinom(15...16, 20...21, 0.6...0.7, true), Res),
    equal(Res, -3.3527... -1.6723).

test(dbinom7a) :- 
    interval(dbinom(11, 20...21, 0.6), Res),
    equal(Res, 0.1341...0.1598).

test(dbinom7b) :- 
    interval(dbinom(11, 20...21, 0.6, false), Res),
    equal(Res, 0.1341...0.1598).

test(dbinom7c) :- 
    interval(dbinom(11, 20...21, 0.6, true), Res),
    equal(Res, -2.0086... -1.8342).

test(dbinom8a) :- 
    interval(dbinom(11, 20, 0.6...0.7), Res),
    equal(Res, 0.0653...0.1598).

test(dbinom8b) :- 
    interval(dbinom(11, 20, 0.6...0.7, false), Res),
    equal(Res, 0.0653...0.1598).

test(dbinom8c) :- 
    interval(dbinom(11, 20, 0.6...0.7, true), Res),
    equal(Res, -2.7277... -1.8342).

test(dbinom9a) :- 
    interval(dbinom(11...12, 20...21, 0.6), Res),
    equal(Res, 0.1341...0.1798).

test(dbinom9b) :- 
    interval(dbinom(11...12, 20...21, 0.6, false), Res),
    equal(Res, 0.1341...0.1798).

test(dbinom9c) :- 
    interval(dbinom(11...12, 20...21, 0.6, true), Res),
    equal(Res, -2.0086... -1.7164).

test(dbinom10a) :- 
    interval(dbinom(11, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0411...0.1598).

test(dbinom10b) :- 
    interval(dbinom(11, 20...21, 0.6...0.7, false), Res),
    equal(Res, 0.0411...0.1598).

test(dbinom10c) :- 
    interval(dbinom(11, 20...21, 0.6...0.7, true), Res),
    equal(Res, -3.1898... -1.8342).

test(qbinom1) :-
    interval(qbinom(0.6, 20, 0.3), Res),
    Res = 6.0.

test(qbinom2) :-
    interval(qbinom(0.6...0.7, 20, 0.3), Res),
    Res = 6.0...7.0.

test(qbinom3) :-
    interval(qbinom(0.6, 20...21, 0.3), Res),
    Res = 6.0...7.0.

test(qbinom4) :-
    interval(qbinom(0.6, 20, 0.3...0.4), Res),
    Res = 6.0...9.0.

test(qbinom5) :-
    interval(qbinom(0.6...0.7, 20...21, 0.3), Res),
    Res = 6.0...7.0.

test(qbinom6) :-
    interval(qbinom(0.6, 20...21, 0.3...0.4), Res),
    Res = 6.0...9.0.

test(qbinom7) :-
    interval(qbinom(0.6...0.7, 20...21, 0.3...0.4), Res),
    Res = 6.0...10.0.

test(qbinom8) :-
    interval(qbinom(0.6, 20, 0.3, true), Res),
    Res = 6.0.

test(qbinom9) :-
    interval(qbinom(0.6, 20, 0.3, false), Res),
    Res = 5.0.

test(qbinom10) :-
    interval(qbinom(0.4...0.6, 20, 0.3, true), Res),
    Res = 5.0...6.0.

test(qbinom11) :-
    interval(qbinom(0.4...0.6, 20, 0.3, false), Res),
    Res = 5.0...6.0.

test(qbinom12) :-
    interval(qbinom(0.6, 20...21, 0.3, true), Res),
    Res = 6.0...7.0.

test(qbinom13) :-
    interval(qbinom(0.6, 20...21, 0.3, false), Res),
    Res = 5.0...6.0.

test(qbinom14) :-
    interval(qbinom(0.6, 20, 0.3...0.4, true), Res),
    Res = 6.0...9.0.

test(qbinom15) :-
    interval(qbinom(0.6, 20, 0.3...0.4, false), Res),
    Res = 5.0...7.0.

test(qbinom16) :-
    interval(qbinom(0.4...0.6, 20...21, 0.3, true), Res),
    Res = 5.0...7.0.

test(qbinom17) :-
    interval(qbinom(0.4...0.6, 20...21, 0.3, false), Res),
    Res = 5.0...7.0.

test(qbinom18) :-
    interval(qbinom(0.6, 20...21, 0.3...0.4, true), Res),
    Res = 6.0...9.0.

test(qbinom19) :-
    interval(qbinom(0.6, 20...21, 0.3...0.4, false), Res),
    Res = 5.0...8.0.

test(qbinom19) :-
    interval(qbinom(0.4...0.6, 20, 0.3...0.4, true), Res),
    Res = 5.0...9.0.

test(qbinom20) :-
    interval(qbinom(0.4...0.6, 20, 0.3...0.4, false), Res),
    Res = 5.0...9.0.

test(qbinom21) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, true), Res),
    equal(Res, 11.9999...15.0001).

test(qbinom22) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, false), Res),
    equal(Res, 10.9999...15.0001).

test(qbinom23) :-
    interval(qbinom(0.6, 20, 0.3, true, false), Res),
    Res = 6.0.

test(qbinom24) :-
    interval(qbinom(-0.51, 20, 0.3, true, true), Res),
    Res = 6.0.

test(qbinom25) :-
    interval(qbinom(0.6, 20, 0.3, false, false), Res),
    Res = 5.0.

test(qbinom26) :-
    interval(qbinom(-0.51, 20, 0.3, false, true), Res),
    Res = 5.0.

test(qbinom26) :-
    interval(qbinom(0.4...0.6, 20, 0.3, true, false), Res),
    Res = 5.0...6.0.

test(qbinom27) :-
    interval(qbinom(-0.92... -0.51, 20, 0.3, true, true), Res),
    Res = 5.0...6.0.

test(qbinom28) :-
    interval(qbinom(0.4...0.6, 20, 0.3, false, false), Res),
    Res = 5.0...6.0.

test(qbinom29) :-
    interval(qbinom(-0.92... -0.51, 20, 0.3, false, true), Res),
    Res = 5.0...6.0.

test(qbinom30) :-
    interval(qbinom(0.6, 20...21, 0.3, true, false), Res),
    Res = 6.0...7.0.

test(qbinom31) :-
    interval(qbinom(-0.51, 20...21, 0.3, true, true), Res),
    Res = 6.0...7.0.

test(qbinom32) :-
    interval(qbinom(0.6, 20...21, 0.3, false, false), Res),
    Res = 5.0...6.0.

test(qbinom33) :-
    interval(qbinom(-0.51, 20...21, 0.3, false, true), Res),
    Res = 5.0...6.0.

test(qbinom34) :-
    interval(qbinom(0.6, 20, 0.3...0.4, true, false), Res),
    Res = 6.0...9.0.

test(qbinom35) :-
    interval(qbinom(-0.51, 20, 0.3...0.4, true, true), Res),
    Res = 6.0...9.0.

test(qbinom36) :-
    interval(qbinom(0.6, 20, 0.3...0.4, false, false), Res),
    Res = 5.0...7.0.

test(qbinom37) :-
    interval(qbinom(-0.51, 20, 0.3...0.4, false, true), Res),
    Res = 5.0...7.0.

test(qbinom38) :-
    interval(qbinom(0.4...0.6, 20...21, 0.3, true, false), Res),
    Res = 5.0...7.0.

test(qbinom39) :-
    interval(qbinom(-0.92... -0.51, 20...21, 0.3, true, true), Res),
    Res = 5.0...7.0.

test(qbinom40) :-
    interval(qbinom(0.4...0.6, 20...21, 0.3, false, false), Res),
    Res = 5.0...7.0.

test(qbinom41) :-
    interval(qbinom(-0.92... -0.51, 20...21, 0.3, false, true), Res),
    Res = 5.0...7.0.

test(qbinom42) :-
    interval(qbinom(0.6, 20...21, 0.3...0.4, true, false), Res),
    Res = 6.0...9.0.

test(qbinom43) :-
    interval(qbinom(-0.51, 20...21, 0.3...0.4, true, true), Res),
    Res = 6.0...9.0.

test(qbinom44) :-
    interval(qbinom(0.6, 20...21, 0.3...0.4, false, false), Res),
    Res = 5.0...8.0.

test(qbinom45) :-
    interval(qbinom(-0.51, 20...21, 0.3...0.4, false, true), Res),
    Res = 5.0...8.0.

test(qbinom46) :-
    interval(qbinom(0.4...0.6, 20, 0.3...0.4, true, false), Res),
    Res = 5.0...9.0.

test(qbinom47) :-
    interval(qbinom(-0.92... -0.51, 20, 0.3...0.4, true, true), Res),
    Res = 5.0...9.0.

test(qbinom48) :-
    interval(qbinom(0.4...0.6, 20, 0.3...0.4, false, false), Res),
    Res = 5.0...9.0.

test(qbinom49) :-
    interval(qbinom(-0.92... -0.51, 20, 0.3...0.4, false, true), Res),
    Res = 5.0...9.0.

test(qbinom50) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, true, false), Res),
    equal(Res, 11.9999...15.0001).

test(qbinom51) :-
    interval(qbinom(-0.69... -0.51, 20...21, 0.6...0.7, true, true), Res),
    equal(Res, 11.9999...15.0001).

test(qbinom52) :-
    interval(qbinom(0.5...0.6, 20...21, 0.6...0.7, false, false), Res),
    equal(Res, 10.9999...15.0001).

test(qbinom53) :-
    interval(qbinom(-0.69... -0.51, 20...21, 0.6...0.7, false, true), Res),
    equal(Res, 10.9999...15.0001).

test(pbinom1) :-
    interval(pbinom(10, 20, 0.6), Res),
    equal(Res, 0.2446...0.2447).

test(pbinom2) :-
    interval(pbinom(10...11, 20, 0.6), Res),
    equal(Res, 0.2446...0.4045).

test(pbinom3) :-
    interval(pbinom(10, 20...21, 0.6), Res),
    equal(Res, 0.1743...0.2447).

test(pbinom4) :-
    interval(pbinom(10, 20, 0.6...0.7), Res),
    equal(Res, 0.0479...0.2447).

test(pbinom5) :-
    interval(pbinom(10...11, 20...21, 0.6), Res),
    equal(Res, 0.1743...0.4045).

test(pbinom6) :-
    interval(pbinom(10...11, 20, 0.6...0.7), Res),
    equal(Res, 0.0479...0.4045).

test(pbinom7) :-
    interval(pbinom(10, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0263...0.2447).

test(pbinom8) :-
    interval(pbinom(10...11, 20...21, 0.6...0.7), Res),
    equal(Res, 0.0263...0.4045).

test(pbinom9) :-
    interval(pbinom(10, 20, 0.6, true), Res),
    equal(Res, 0.2446...0.2447).

test(pbinom10) :-
    interval(pbinom(10, 20, 0.6, false), Res),
    equal(Res, 0.7553...0.7554).

test(pbinom11) :-
    interval(pbinom(10...12, 20, 0.6, true), Res),
    equal(Res, 0.2446...0.5842).

test(pbinom12) :-
    interval(pbinom(10...12, 20, 0.6, false), Res),
    equal(Res, 0.4158...0.7554).

test(pbinom13) :-
    interval(pbinom(10...12, 20...21, 0.6, true), Res),
    equal(Res, 0.1743...0.5842).

test(pbinom14) :-
    interval(pbinom(10...12, 20...21, 0.6, false), Res),
    equal(Res, 0.4158...0.8257).

test(pbinom15) :-
    interval(pbinom(10...12, 20, 0.6...0.7, true), Res),
    equal(Res, 0.0479...0.5842).

test(pbinom16) :-
    interval(pbinom(10...12, 20, 0.6...0.7, false), Res),
    equal(Res, 0.4158...0.9521).

test(pbinom17) :-
    interval(pbinom(10, 20...21, 0.6...0.7, true), Res),
    equal(Res, 0.0263...0.2447).

test(pbinom18) :-
    interval(pbinom(10, 20...21, 0.6...0.7, false), Res),
    equal(Res, 0.7553...0.9737).

test(pbinom18) :-
    interval(pbinom(10...12, 20...21, 0.6...0.7, true), Res),
    equal(Res, 0.0263...0.5842).

test(pbinom19) :-
    interval(pbinom(10...12, 20...21, 0.6...0.7, false), Res),
    equal(Res, 0.4158...0.9737).

test(pbinom20) :-
    interval(pbinom(10, 20, 0.6, true, false), Res),
    equal(Res, 0.2446...0.2447). 

test(pbinom21) :-
    interval(pbinom(10, 20, 0.6, true, true), Res),
    equal(Res, -1.4079... -1.4078).

test(pbinom22) :-
    interval(pbinom(10, 20, 0.6, false, false), Res),
    equal(Res, 0.7553...0.7554).

test(pbinom23) :-
    interval(pbinom(10, 20, 0.6, false, true), Res),
    equal(Res, -0.2806... -0.2805).

test(pbinom24) :-
    interval(pbinom(10...12, 20, 0.6, true, false), Res),
    equal(Res, 0.2446...0.5842).

test(pbinom25) :-
    interval(pbinom(10...12, 20, 0.6, true, true), Res),
    equal(Res, -1.4079... -0.5376).

test(pbinom26) :-
    interval(pbinom(10...12, 20, 0.6, false, false), Res),
    equal(Res, 0.4158...0.7554).

test(pbinom27) :-
    interval(pbinom(10...12, 20, 0.6, false, true), Res),
    equal(Res, -0.8774... -0.2805).

test(pbinom28) :-
    interval(pbinom(10...12, 20...21, 0.6, true, false), Res),
    equal(Res, 0.1743...0.5842).

test(pbinom29) :-
    interval(pbinom(10...12, 20...21, 0.6, true, true), Res),
    equal(Res, -1.7466... -0.5376).

test(pbinom30) :-
    interval(pbinom(10...12, 20...21, 0.6, false, false), Res),
    equal(Res, 0.4158...0.8257).

test(pbinom31) :-
    interval(pbinom(10...12, 20...21, 0.6, false, true), Res),
    equal(Res, -0.8774... -0.1916).

test(pbinom32) :-
    interval(pbinom(10...12, 20, 0.6...0.7, true, false), Res),
    equal(Res, 0.0479...0.5842).

test(pbinom33) :-
    interval(pbinom(10...12, 20, 0.6...0.7, true, true), Res),
    equal(Res, -3.0374... -0.5376).

test(pbinom34) :-
    interval(pbinom(10...12, 20, 0.6...0.7, false, false), Res),
    equal(Res, 0.4158...0.9521).

test(pbinom35) :-
    interval(pbinom(10...12, 20, 0.6...0.7, false, true), Res),
    equal(Res, -0.8774... -0.0491).

test(pbinom36) :-
    interval(pbinom(10, 20...21, 0.6...0.7, true, false), Res),
    equal(Res, 0.0263...0.2447).

test(pbinom37) :-
    interval(pbinom(10, 20...21, 0.6...0.7, true, true), Res),
    equal(Res, -3.6348... -1.4078).

test(pbinom38) :-
    interval(pbinom(10, 20...21, 0.6...0.7, false, false), Res),
    equal(Res, 0.7553...0.9737).

test(pbinom39) :-
    interval(pbinom(10, 20...21, 0.6...0.7, false, true), Res),
    equal(Res, -0.2806... -0.0267).

test(pbinom40) :-
    interval(pbinom(10...12, 20...21, 0.6...0.7, true, false), Res),
    equal(Res, 0.0263...0.5842).

test(pbinom41) :-
    interval(pbinom(10...12, 20...21, 0.6...0.7, true, true), Res),
    equal(Res, -3.6348... -0.5376).

test(pbinom42) :-
    interval(pbinom(10...12, 20...21, 0.6...0.7, false, false), Res),
    equal(Res, 0.4158...0.9737).

test(pbinom43) :-
    interval(pbinom(10...12, 20...21, 0.6...0.7, false, true), Res),
    equal(Res, -0.8774... -0.0267).

:- end_tests(binom).

:- begin_tests(normal).

test(pnorm1) :-
    interval(pnorm(0.5), Res),
    equal(Res, 0.6914...0.6915).

test(pnorm2) :-
    interval(pnorm(0.5...0.7), Res),
    equal(Res, 0.6914...0.7581).

test(pnorm3) :-
    interval(pnorm(90, 100, 10), Res),
    equal(Res, 0.1586...0.1587).

test(pnorm4) :-
    interval(pnorm(90...92, 100, 10), Res),
    equal(Res, 0.1586...0.2119).

test(pnorm5) :-
    interval(pnorm(90, 100...102, 10), Res),
    equal(Res, 0.1150...0.1587).

test(pnorm6) :-
    interval(pnorm(90, 100, 10...12), Res),
    equal(Res, 0.1586...0.2024).

test(pnorm7) :-
    interval(pnorm(90...92, 100...102, 10), Res),
    equal(Res, 0.1150...0.2119).

test(pnorm8) :-
    interval(pnorm(90...92, 100, 10...11), Res),
    equal(Res, 0.1586...0.2336).

test(pnorm8) :-
    interval(pnorm(90, 100...102, 10...11), Res),
    equal(Res, 0.1150...0.1817).

test(pnorm9) :-
    interval(pnorm(90...91, 100...101, 10...11), Res),
    equal(Res, 0.1356...0.2067).

test(pnorm10) :-
    interval(pnorm(90, 100, 10, true), Res),
    equal(Res, 0.1586...0.1587).

test(pnorm11) :-
    interval(pnorm(90, 100, 10, false), Res),
    equal(Res, 0.8413...0.8414).

test(pnorm12) :-
    interval(pnorm(90...92, 100, 10, true), Res),
    equal(Res, 0.1586...0.2119).

test(pnorm13) :-
    interval(pnorm(90...92, 100, 10, false), Res),
    equal(Res, 0.7881...0.8414).

test(pnorm14) :-
    interval(pnorm(90, 100...102, 10, true), Res),
    equal(Res, 0.1150...0.1587).

test(pnorm15) :-
    interval(pnorm(90, 100...102, 10, false), Res),
    equal(Res, 0.8413...0.8850).

test(pnorm16) :-
    interval(pnorm(90, 100, 10...12, true), Res),
    equal(Res, 0.1586...0.2024).

test(pnorm17) :-
    interval(pnorm(90, 100, 10...12, false), Res),
    equal(Res, 0.7976...0.8414).

test(pnorm18) :-
    interval(pnorm(90...92, 100...102, 10, true), Res),
    equal(Res, 0.1150...0.2119).

test(pnorm19) :-
    interval(pnorm(90...92, 100...102, 10, false), Res),
    equal(Res, 0.7881...0.8850).

test(pnorm20) :-
    interval(pnorm(90...92, 100, 10...11, true), Res),
    equal(Res, 0.1586...0.2336).

test(pnorm21) :-
    interval(pnorm(90...92, 100, 10...11, false), Res),
    equal(Res, 0.7664...0.8414).

test(pnorm22) :-
    interval(pnorm(90, 100...102, 10...11, true), Res),
    equal(Res, 0.1150...0.1817).

test(pnorm23) :-
    interval(pnorm(90, 100...102, 10...11, false), Res),
    equal(Res, 0.8183...0.8850).

test(pnorm24) :-
    interval(pnorm(90...91, 100...101, 10...11, true), Res),
    equal(Res, 0.1356...0.2067).

test(pnorm25) :-
    interval(pnorm(90...91, 100...101, 10...11, false), Res),
    equal(Res, 0.7933...0.8644).

test(pnorm26) :-
    interval(pnorm(90, 100, 10, true, false), Res),
    equal(Res, 0.1586...0.1587).

test(pnorm27) :-
    interval(pnorm(90, 100, 10, true, true), Res),
    equal(Res, -1.8411... -1.8410).

test(pnorm28) :-
    interval(pnorm(90, 100, 10, false, false), Res),
    equal(Res, 0.8413...0.8414).

test(pnorm29) :-
    interval(pnorm(90, 100, 10, false, true), Res),
    equal(Res, -0.1728... -0.1727).

test(pnorm30) :-
    interval(pnorm(90...92, 100, 10, true, false), Res),
    equal(Res, 0.1586...0.2119).

test(pnorm31) :-
    interval(pnorm(90...92, 100, 10, true, true), Res),
    equal(Res, -1.8411... -1.5518).

test(pnorm32) :-
    interval(pnorm(90...92, 100, 10, false, false), Res),
    equal(Res, 0.7881...0.8414).

test(pnorm33) :-
    interval(pnorm(90...92, 100, 10, false, true), Res),
    equal(Res, -0.2381... -0.1727).

test(pnorm34) :-
    interval(pnorm(90, 100...102, 10, true, false), Res),
    equal(Res, 0.1150...0.1587).

test(pnorm35) :-
    interval(pnorm(90, 100...102, 10, true, true), Res),
    equal(Res, -2.1623... -1.8410).

test(pnorm36) :-
    interval(pnorm(90, 100...102, 10, false, false), Res),
    equal(Res, 0.8413...0.8850).

test(pnorm37) :-
    interval(pnorm(90, 100...102, 10, false, true), Res),
    equal(Res, -0.1728... -0.1222).

test(pnorm38) :-
    interval(pnorm(90, 100, 10...12, true, false), Res),
    equal(Res, 0.1586...0.2024).

test(pnorm39) :-
    interval(pnorm(90, 100, 10...12, true, true), Res),
    equal(Res, -1.8411... -1.5978).

test(pnorm40) :-
    interval(pnorm(90, 100, 10...12, false, false), Res),
    equal(Res, 0.7976...0.8414).

test(pnorm41) :-
    interval(pnorm(90, 100, 10...12, false, true), Res),
    equal(Res, -0.2261... -0.1727).

test(pnorm42) :-
    interval(pnorm(90...92, 100...102, 10, true, false), Res),
    equal(Res, 0.1150...0.2119).

test(pnorm43) :-
    interval(pnorm(90...92, 100...102, 10, true, true), Res),
    equal(Res, -2.1623... -1.5518).

test(pnorm44) :-
    interval(pnorm(90...92, 100...102, 10, false, false), Res),
    equal(Res, 0.7881...0.8850).

test(pnorm45) :-
    interval(pnorm(90...92, 100...102, 10, false, true), Res),
    equal(Res, -0.2381... -0.1222).

test(pnorm46) :-
    interval(pnorm(90...92, 100, 10...11, true, false), Res),
    equal(Res, 0.1586...0.2336).

test(pnorm47) :-
    interval(pnorm(90...92, 100, 10...11, true, true), Res),
    equal(Res, -1.8411... -1.4544).

test(pnorm48) :-
    interval(pnorm(90...92, 100, 10...11, false, false), Res),
    equal(Res, 0.7664...0.8414).

test(pnorm49) :-
    interval(pnorm(90...92, 100, 10...11, false, true), Res),
    equal(Res, -0.2660... -0.1727).

test(pnorm50) :-
    interval(pnorm(90, 100...102, 10...11, true, false), Res),
    equal(Res, 0.1150...0.1817).

test(pnorm51) :-
    interval(pnorm(90, 100...102, 10...11, true, true), Res),
    equal(Res, -2.1623... -1.7056).

test(pnorm52) :-
    interval(pnorm(90, 100...102, 10...11, false, false), Res),
    equal(Res, 0.8183...0.8850).

test(pnorm53) :-
    interval(pnorm(90, 100...102, 10...11, false, true), Res),
    equal(Res, -0.2005... -0.1222).

test(pnorm54) :-
    interval(pnorm(90...91, 100...101, 10...11, true, false), Res),
    equal(Res, 0.1356...0.2067).

test(pnorm55) :-
    interval(pnorm(90...91, 100...101, 10...11, true, true), Res),
    equal(Res, -1.9976... -1.5768).

test(pnorm56) :-
    interval(pnorm(90...91, 100...101, 10...11, false, false), Res),
    equal(Res, 0.7933...0.8644).

test(pnorm57) :-
    interval(pnorm(90...91, 100...101, 10...11, false, true), Res),
    equal(Res, -0.2315... -0.1457).

test(qnorm1) :-
    interval(qnorm(0.6), Res),
    equal(Res, 0.2533...0.2534).

test(qnorm2) :-
    interval(qnorm(0.6...0.7), Res),
    equal(Res, 0.2533...0.5245).

test(qnorm3) :-
    interval(qnorm(0.6, 100, 10), Res),
    equal(Res, 102.5334...102.5335).

test(qnorm4a) :-
    interval(qnorm(0.6...0.8, 100, 10), Res),
    equal(Res, 102.5334...108.4163).

test(qnorm4b) :-
    interval(qnorm(0.3...0.4, 100, 10), Res),
    equal(Res, 94.7559...97.4666).

test(qnorm4c) :-
    interval(qnorm(0.4...0.6, 100, 10), Res),
    equal(Res, 97.4665...102.5335).

test(qnorm5a) :-
    interval(qnorm(0.6, 100...101, 10), Res),
    equal(Res, 102.5334...103.5335).

test(qnorm5b) :-
    interval(qnorm(0.4, 100...101, 10), Res),
    equal(Res, 97.4665...98.4666).

test(qnorm6a) :-
    interval(qnorm(0.6, 100, 10...11), Res),
    equal(Res, 102.5334...102.7869).

test(qnorm6b) :-
    interval(qnorm(0.4, 100, 10...11), Res),
    equal(Res, 97.2131...97.4666).

test(qnorm7a) :-
    interval(qnorm(0.6...0.8, 100...101, 10), Res),
    equal(Res, 102.5334...109.4163).

test(qnorm7b) :-
    interval(qnorm(0.3...0.4, 100...101, 10), Res),
    equal(Res, 94.7559...98.4666).

test(qnorm7c) :-
    interval(qnorm(0.4...0.6, 100...101, 10), Res),
    equal(Res, 97.4665...103.5335).

test(qnorm8a) :-
    interval(qnorm(0.6...0.8, 100, 10...11), Res),
    equal(Res, 102.5334...109.2579).

test(qnorm8b) :-
    interval(qnorm(0.3...0.4, 100, 10...11), Res),
    equal(Res, 94.2315...97.4666).

test(qnorm8c) :-
    interval(qnorm(0.4...0.6, 100, 10...11), Res),
    equal(Res, 97.4665...102.7869).

test(qnorm9a) :-
    interval(qnorm(0.6...0.7, 100...101, 10...11), Res),
    equal(Res, 102.5334...106.7685).

test(qnorm9b) :-
    interval(qnorm(0.3...0.4, 100...101, 10...11), Res),
    equal(Res, 94.2315...98.4666).

test(qnorm9c) :-
    interval(qnorm(0.4...0.6, 100...101, 10...11), Res),
    equal(Res, 97.4665...103.7869).

test(qnorm10) :-
    interval(qnorm(0.6, 100, 10, true), Res),
    equal(Res, 102.5334...102.5335).

test(qnorm11) :-
    interval(qnorm(0.6, 100, 10, false), Res),
    equal(Res, 97.4665...97.4666).

test(qnorm12a) :-
    interval(qnorm(0.6...0.8, 100, 10, true), Res),
    equal(Res, 102.5334...108.4163).

test(qnorm12b) :-
    interval(qnorm(0.3...0.4, 100, 10, true), Res),
    equal(Res, 94.7559...97.4666).

test(qnorm12c) :-
    interval(qnorm(0.4...0.6, 100, 10, true), Res),
    equal(Res, 97.4665...102.5335).

test(qnorm13a) :-
    interval(qnorm(0.6...0.8, 100, 10, false), Res),
    equal(Res, 91.5837...97.4666).

test(qnorm13b) :-
    interval(qnorm(0.3...0.4, 100, 10, false), Res),
    equal(Res, 102.5334...105.2441).

test(qnorm13c) :-
    interval(qnorm(0.4...0.6, 100, 10, false), Res),
    equal(Res, 97.4665...102.5335).

test(qnorm14a) :-
    interval(qnorm(0.6, 100...101, 10, true), Res),
    equal(Res, 102.5334...103.5335).

test(qnorm14b) :-
    interval(qnorm(0.4, 100...101, 10, true), Res),
    equal(Res, 97.4665...98.4666).

test(qnorm15a) :-
    interval(qnorm(0.6, 100...101, 10, false), Res),
    equal(Res, 97.4665...98.4666).

test(qnorm15b) :-
    interval(qnorm(0.4, 100...101, 10, false), Res),
    equal(Res, 102.5334...103.5335).

test(qnorm16a) :-
    interval(qnorm(0.6, 100, 10...11, true), Res),
    equal(Res, 102.5334...102.7869).

test(qnorm16b) :-
    interval(qnorm(0.4, 100, 10...11, true), Res),
    equal(Res, 97.2131...97.4666).

test(qnorm17a) :-
    interval(qnorm(0.6, 100, 10...11, false), Res),
    equal(Res, 97.2131...97.4666).

test(qnorm17b) :-
    interval(qnorm(0.4, 100, 10...11, false), Res),
    equal(Res, 102.5334...102.7869).

test(qnorm18a) :-
    interval(qnorm(0.6...0.8, 100...101, 10, true), Res),
    equal(Res, 102.5334...109.4163).

test(qnorm18b) :-
    interval(qnorm(0.3...0.4, 100...101, 10, true), Res),
    equal(Res, 94.7559...98.4666).

test(qnorm18c) :-
    interval(qnorm(0.4...0.6, 100...101, 10, true), Res),
    equal(Res, 97.4665...103.5335).

test(qnorm19a) :-
    interval(qnorm(0.6...0.8, 100...101, 10, false), Res),
    equal(Res, 91.5837...98.4666).

test(qnorm19b) :-
    interval(qnorm(0.3...0.4, 100...101, 10, false), Res),
    equal(Res, 102.5334...106.2441).

test(qnorm19c) :-
    interval(qnorm(0.4...0.6, 100...101, 10, false), Res),
    equal(Res, 97.4665...103.5335).

test(qnorm20a) :-
    interval(qnorm(0.6...0.8, 100, 10...11, true), Res),
    equal(Res, 102.5334...109.2579).

test(qnorm20b) :-
    interval(qnorm(0.3...0.4, 100, 10...11, true), Res),
    equal(Res, 94.2315...97.4666).

test(qnorm20c) :-
    interval(qnorm(0.4...0.6, 100, 10...11, true), Res),
    equal(Res, 97.4665...102.7869).

test(qnorm21a) :-
    interval(qnorm(0.6...0.8, 100, 10...11, false), Res),
    equal(Res, 90.7421...97.4666).

test(qnorm21b) :-
    interval(qnorm(0.3...0.4, 100, 10...11, false), Res),
    equal(Res, 102.5334...105.7685).

test(qnorm21c) :-
    interval(qnorm(0.4...0.6, 100, 10...11, false), Res),
    equal(Res, 97.2131...102.5335).

test(qnorm22a) :-
    interval(qnorm(0.6...0.7, 100...101, 10...11, true), Res),
    equal(Res, 102.5334...106.7685).

test(qnorm22b) :-
    interval(qnorm(0.3...0.4, 100...101, 10...11, true), Res),
    equal(Res, 94.2315...98.4666).

test(qnorm22c) :-
    interval(qnorm(0.4...0.6, 100...101, 10...11, true), Res),
    equal(Res, 97.4665...103.7869).

test(qnorm23a) :-
    interval(qnorm(0.6...0.7, 100...101, 10...11, false), Res),
    equal(Res, 94.2315...98.4666).

test(qnorm23b) :-
    interval(qnorm(0.3...0.4, 100...101, 10...11, false), Res),
    equal(Res, 102.5334...106.7685).

test(qnorm23c) :-
    interval(qnorm(0.4...0.6, 100...101, 10...11, false), Res),
    equal(Res, 97.2131...103.5335).

test(qnorm24) :-
    interval(qnorm(0.6, 100, 10, true, false), Res),
    equal(Res, 102.5334...102.5335).

test(qnorm25) :-
    interval(qnorm(-0.51, 100, 10, true, true), Res),
    equal(Res, 102.5463...102.5464).
    
test(qnorm26) :-
    interval(qnorm(0.6, 100, 10, false, false), Res),
    equal(Res, 97.4665...97.4666).

test(qnorm27) :-
    interval(qnorm(-0.51, 100, 10, false, true), Res),
    equal(Res, 97.4536...97.4537).

test(qnorm28a) :-
    interval(qnorm(0.6...0.8, 100, 10, true, false), Res),
    equal(Res, 102.5334...108.4163).

test(qnorm29a) :-
    interval(qnorm(-0.51... -0.22, 100, 10, true, true), Res),
    equal(Res, 102.5463...108.5066).

test(qnorm28b) :-
    interval(qnorm(0.3...0.4, 100, 10, true, false), Res),
    equal(Res, 94.7559...97.4666).

test(qnorm29b) :-
    interval(qnorm(-1.20... -0.92, 100, 10, true, true), Res),
    equal(Res, 94.7903...97.4282).

test(qnorm28c) :-
    interval(qnorm(0.4...0.6, 100, 10, true, false), Res),
    equal(Res, 97.4665...102.5335).

test(qnorm29c) :-
    interval(qnorm(-0.92... -0.51, 100, 10, true, true), Res),
    equal(Res, 97.4281...102.5464).

test(qnorm30a) :-
    interval(qnorm(0.6...0.8, 100, 10, false, false), Res),
    equal(Res, 91.5837...97.4666).

test(qnorm31a) :-
    interval(qnorm(-0.51... -0.22, 100, 10, false, true), Res),
    equal(Res, 91.4934...97.4537).

test(qnorm30b) :-
    interval(qnorm(0.3...0.4, 100, 10, false, false), Res),
    equal(Res, 102.5334...105.2441).

test(qnorm31b) :-
    interval(qnorm(-1.20... -0.92, 100, 10, false, true), Res),
    equal(Res, 102.5718...105.2097).

test(qnorm30c) :-
    interval(qnorm(0.4...0.6, 100, 10, false, false), Res),
    equal(Res, 97.4665...102.5335).

test(qnorm31c) :-
    interval(qnorm(-0.92... -0.51, 100, 10, false, true), Res),
    equal(Res, 97.4536...102.5719).

test(qnorm32a) :-
    interval(qnorm(0.6, 100...101, 10, true, false), Res),
    equal(Res, 102.5334...103.5335).

test(qnorm33a) :-
    interval(qnorm(-0.51, 100...101, 10, true, true), Res),
    equal(Res, 102.5463...103.5464).

test(qnorm32b) :-
    interval(qnorm(0.4, 100...101, 10, true, false), Res),
    equal(Res, 97.4665...98.4666).

test(qnorm33b) :-
    interval(qnorm(-0.92, 100...101, 10, true, true), Res),
    equal(Res, 97.4281...98.4282).

test(qnorm34a) :-
    interval(qnorm(0.6, 100...101, 10, false, false), Res),
    equal(Res, 97.4665...98.4666).

test(qnorm35a) :-
    interval(qnorm(-0.51, 100...101, 10, false, true), Res),
    equal(Res, 97.4536...98.4537).

test(qnorm34b) :-
    interval(qnorm(0.4, 100...101, 10, false, false), Res),
    equal(Res, 102.5334...103.5335).

test(qnorm35b) :-
    interval(qnorm(-0.92, 100...101, 10, false, true), Res),
    equal(Res, 102.5718...103.5719).

test(qnorm36a) :-
    interval(qnorm(0.6, 100, 10...11, true, false), Res),
    equal(Res, 102.5334...102.7869).

test(qnorm37a) :-
    interval(qnorm(-0.51, 100, 10...11, true, true), Res),
    equal(Res, 102.5463...102.8010).

test(qnorm36b) :-
    interval(qnorm(0.4, 100, 10...11, true, false), Res),
    equal(Res, 97.2131...97.4666).

test(qnorm37b) :-
    interval(qnorm(-0.92, 100, 10...11, true, true), Res),
    equal(Res, 97.1709...97.4282).

test(qnorm38a) :-
    interval(qnorm(0.6, 100, 10...11, false, false), Res),
    equal(Res, 97.2131...97.4666).

test(qnorm39a) :-
    interval(qnorm(-0.51, 100, 10...11, false, true), Res),
    equal(Res, 97.1990...97.4537).

test(qnorm38b) :-
    interval(qnorm(0.4, 100, 10...11, false, false), Res),
    equal(Res, 102.5334...102.7869).

test(qnorm39b) :-
    interval(qnorm(-0.92, 100, 10...11, false, true), Res),
    equal(Res, 102.5718...102.8291).

test(qnorm40a) :-
    interval(qnorm(0.6...0.8, 100...101, 10, true, false), Res),
    equal(Res, 102.5334...109.4163).
    
test(qnorm41a) :-
    interval(qnorm(-0.51... -0.22, 100...101, 10, true, true), Res),
    equal(Res, 102.5463...109.5066).

test(qnorm40b) :-
    interval(qnorm(0.3...0.4, 100...101, 10, true, false), Res),
    equal(Res, 94.7559...98.4666).

test(qnorm41b) :-
    interval(qnorm(-1.20... -0.92, 100...101, 10, true, true), Res),
    equal(Res, 94.7903...98.4282).

test(qnorm40c) :-
    interval(qnorm(0.4...0.6, 100...101, 10, true, false), Res),
    equal(Res, 97.4665...103.5335).

test(qnorm41c) :-
    interval(qnorm(-0.92... -0.51, 100...101, 10, true, true), Res),
    equal(Res, 97.4281...103.5464).

test(qnorm42a) :-
    interval(qnorm(0.6...0.8, 100...101, 10, false, false), Res),
    equal(Res, 91.5837...98.4666).

test(qnorm43a) :-
    interval(qnorm(-0.51... -0.22, 100...101, 10, false, true), Res),
    equal(Res, 91.4934...98.4537).

test(qnorm42b) :-
    interval(qnorm(0.3...0.4, 100...101, 10, false, false), Res),
    equal(Res, 102.5334...106.2441).

test(qnorm43b) :-
    interval(qnorm(-1.20... -0.92, 100...101, 10, false, true), Res),
    equal(Res, 102.5718...106.2097).

test(qnorm42c) :-
    interval(qnorm(0.4...0.6, 100...101, 10, false, false), Res),
    equal(Res, 97.4665...103.5335).

test(qnorm43c) :-
    interval(qnorm(-0.92... -0.51, 100...101, 10, false, true), Res),
    equal(Res, 97.4536...103.5719).

test(qnorm44a) :-
    interval(qnorm(0.6...0.8, 100, 10...11, true, false), Res),
    equal(Res, 102.5334...109.2579).

test(qnorm45a) :-
    interval(qnorm(-0.51... -0.22, 100, 10...11, true, true), Res),
    equal(Res, 102.5463...109.3572).

test(qnorm44b) :-
    interval(qnorm(0.3...0.4, 100, 10...11, true, false), Res),
    equal(Res, 94.2315...97.4666).

test(qnorm45b) :-
    interval(qnorm(-1.20... -0.92, 100, 10...11, true, true), Res),
    equal(Res, 94.2693...97.4282).

test(qnorm44c) :-
    interval(qnorm(0.4...0.6, 100, 10...11, true, false), Res),
    equal(Res, 97.4665...102.7869).

test(qnorm45c) :-
    interval(qnorm(-0.92... -0.51, 100, 10...11, true, true), Res),
    equal(Res, 97.4281...102.8010).

test(qnorm46a) :-
    interval(qnorm(0.6...0.8, 100, 10...11, false, false), Res),
    equal(Res, 90.7421...97.4666).

test(qnorm47a) :-
    interval(qnorm(-0.51... -0.22, 100, 10...11, false, true), Res),
    equal(Res, 90.6428...97.4537).

test(qnorm46b) :-
    interval(qnorm(0.3...0.4, 100, 10...11, false, false), Res),
    equal(Res, 102.5334...105.7685).

test(qnorm47b) :-
    interval(qnorm(-1.20... -0.92, 100, 10...11, false, true), Res),
    equal(Res, 102.5718...105.7307).

test(qnorm46c) :-
    interval(qnorm(0.4...0.6, 100, 10...11, false, false), Res),
    equal(Res, 97.2131...102.5335).

test(qnorm47c) :-
    interval(qnorm(-0.92... -0.51, 100, 10...11, false, true), Res),
    equal(Res, 97.1990...102.5719).

test(qnorm48a) :-
    interval(qnorm(0.6...0.7, 100...101, 10...11, true, false), Res),
    equal(Res, 102.5334...106.7685).

test(qnorm49a) :-
    interval(qnorm(-0.51... -0.36, 100...101, 10...11, true, true), Res),
    equal(Res, 102.5463...106.6951).

test(qnorm48b) :-
    interval(qnorm(0.3...0.4, 100...101, 10...11, true, false), Res),
    equal(Res, 94.2315...98.4666).

test(qnorm49b) :-
    interval(qnorm(-1.20... -0.92, 100...101, 10...11, true, true), Res),
    equal(Res, 94.2693...98.4282).

test(qnorm48c) :-
    interval(qnorm(0.4...0.6, 100...101, 10...11, true, false), Res),
    equal(Res, 97.4665...103.7869).

test(qnorm49c) :-
    interval(qnorm(-0.92... -0.51, 100...101, 10...11, true, true), Res),
    equal(Res, 97.4281...103.8010).

test(qnorm50a) :-
    interval(qnorm(0.6...0.7, 100...101, 10...11, false, false), Res),
    equal(Res, 94.2315...98.4666).

test(qnorm51a) :-
    interval(qnorm(-0.51... -0.36, 100...101, 10...11, false, true), Res),
    equal(Res, 94.3049...98.4537).

test(qnorm50b) :-
    interval(qnorm(0.3...0.4, 100...101, 10...11, false, false), Res),
    equal(Res, 102.5334...106.7685).

test(qnorm51b) :-
    interval(qnorm(-1.20... -0.92, 100...101, 10...11, false, true), Res),
    equal(Res, 102.5718...106.7307).

test(qnorm50c) :-
    interval(qnorm(0.4...0.6, 100...101, 10...11, false, false), Res),
    equal(Res, 97.2131...103.5335).

test(qnorm51c) :-
    interval(qnorm(-0.91... -0.51, 100...101, 10...11, false, true), Res),
    equal(Res, 97.1990...103.4682).

test(dnorm1) :-
    interval(dnorm(0.5), Res),
    equal(Res, 0.3520...0.3521).

test(dnorm2) :-
    interval(dnorm(0.5...0.6), Res),
    equal(Res, 0.3332...0.3521).

test(dnorm3) :-
    interval(dnorm(0.5, false), Res),
    equal(Res, 0.3520...0.3521).

test(dnorm4) :-
    interval(dnorm(0.5, true), Res),
    equal(Res, -1.0440... -1.0439).

test(dnorm5) :-
    interval(dnorm(90, 100, 10), Res),
    equal(Res, 0.0241...0.0242).

test(dnorm6) :-
    interval(dnorm(90...92, 100, 10), Res),
    equal(Res, 0.0241...0.029).

test(dnorm7) :-
    interval(dnorm(90, 100...102, 10), Res),
    equal(Res, 0.0194...0.0242).

test(dnorm8) :-
    interval(dnorm(90, 100, 10...12), Res),
    equal(Res, 0.0201...0.0282).

test(dnorm9) :-
    interval(dnorm(90...92, 100...102, 10), Res),
    equal(Res, 0.0194...0.029).

test(dnorm10) :-
    interval(dnorm(90...92, 100, 10...11), Res),
    equal(Res, 0.0219...0.0307).

test(dnorm11) :-
    interval(dnorm(90, 100...102, 10...11), Res),
    equal(Res, 0.0176...0.0264).

test(dnorm12) :-
    interval(dnorm(90...91, 100...101, 10...11), Res),
    equal(Res, 0.0198...0.0286).

test(dnorm13) :-
    interval(dnorm(110...111, 100...101, 10...11), Res),
    equal(Res, 0.0198...0.0286).

test(dnorm14) :-
    interval(dnorm(99...102, 100...101, 10...11), Res),
    equal(Res, 0.0355...0.0399).

test(dnorm15) :-
    interval(dnorm(90, 100, 10, false), Res),
    equal(Res, 0.0241...0.0242).

test(dnorm16) :-
    interval(dnorm(90, 100, 10, true), Res),
    equal(Res, -3.7216... -3.7215).

test(dnorm17) :-
    interval(dnorm(90...92, 100, 10, false), Res),
    equal(Res, 0.0241...0.029).

test(dnorm18) :-
    interval(dnorm(90...92, 100, 10, true), Res),
    equal(Res, -3.7216... -3.5415).

test(dnorm19) :-
    interval(dnorm(90, 100...102, 10, false), Res),
    equal(Res, 0.0194...0.0242).

test(dnorm20) :-
    interval(dnorm(90, 100...102, 10, true), Res),
    equal(Res, -3.9416... -3.7215).

test(dnorm21) :-
    interval(dnorm(90, 100, 10...12, false), Res),
    equal(Res, 0.0201...0.0282).

test(dnorm22) :-
    interval(dnorm(90, 100, 10...12, true), Res),
    equal(Res, -3.9039... -3.5687).

test(dnorm23) :-
    interval(dnorm(90...92, 100...102, 10, false), Res),
    equal(Res, 0.0194...0.029).

test(dnorm24) :-
    interval(dnorm(90...92, 100...102, 10, true), Res),
    equal(Res, -3.9416... -3.5415).

test(dnorm25) :-
    interval(dnorm(90...92, 100, 10...11, false), Res),
    equal(Res, 0.0219...0.0307).

test(dnorm26) :-
    interval(dnorm(90...92, 100, 10...11, true), Res),
    equal(Res, -3.8169... -3.4859).

test(dnorm27) :-
    interval(dnorm(90, 100...102, 10...11, false), Res),
    equal(Res, 0.0176...0.0264).

test(dnorm28) :-
    interval(dnorm(90, 100...102, 10...11, true), Res),
    equal(Res, -4.0369... -3.6347).

test(dnorm29) :-
    interval(dnorm(90...91, 100...101, 10...11, false), Res),
    equal(Res, 0.0198...0.0286).

test(dnorm30) :-
    interval(dnorm(90...91, 100...101, 10...11, true), Res),
    equal(Res, -3.9219... -3.5562).

test(dnorm31) :-
    interval(dnorm(110...111, 100...101, 10...11, false), Res),
    equal(Res, 0.0198...0.0286).

test(dnorm32) :-
    interval(dnorm(110...111, 100...101, 10...11, true), Res),
    equal(Res, -3.9219... -3.5562).

test(dnorm33) :-
    interval(dnorm(99...102, 100...101, 10...11, false), Res),
    equal(Res, 0.0355...0.0399).
    
test(dnorm34) :-
    interval(dnorm(99...102, 100...101, 10...11, true), Res),
    equal(Res, -3.3369... -3.2215).

:- end_tests(normal).

:- begin_tests(t).

test(pt1) :-
    interval(pt(-0.5, 5), Res),
    equal(Res, 0.3191...0.3192).

test(pt2) :-
    interval(pt(-0.5... -0.2, 5), Res),
    equal(Res, 0.3191...0.4247).

test(pt3) :-
    interval(pt(-0.5, 5...6), Res),
    equal(Res, 0.3191...0.3175).

test(pt4) :-
    interval(pt(-0.5... -0.2, 5...6), Res),
    equal(Res, 0.3191...0.4241).

test(pt5) :-
    interval(pt(-0.5, 5, true), Res),
    equal(Res, 0.3191...0.3192).

test(pt6) :-
    interval(pt(-0.5, 5, false), Res),
    equal(Res, 0.6808...0.6809).

test(pt7) :-
    interval(pt(-0.5... -0.2, 5, true), Res),
    equal(Res, 0.3191...0.4247).

test(pt8) :-
    interval(pt(-0.5... -0.2, 5, false), Res),
    equal(Res, 0.5753...0.6809).

test(pt9) :-
    interval(pt(-0.5, 5...6, true), Res),
    equal(Res, 0.3174...0.3192).

test(pt10) :-
    interval(pt(-0.5, 5...6, false), Res),
    equal(Res, 0.6808...0.6826).

test(pt11) :-
    interval(pt(-0.5... -0.2, 5...6, true), Res),
    equal(Res, 0.3174...0.4247).

test(pt12) :-
    interval(pt(0.5...1, 2...5, false), Res),
    equal(Res, 0.1816...0.3334).

test(pt13) :-
    interval(pt(-0.5...0.2, 2...5, true), Res),
    equal(Res, 0.4999...0.6809).

test(pt14) :-
    interval(pt(-0.5...0.2, 2...5, false), Res),
    equal(Res, 0.3191...0.5001).

test(pt15) :-
    interval(pt(-0.5, 5, true, false), Res),
    equal(Res, 0.3191...0.3192).

test(pt16) :-
    interval(pt(-0.5, 5, true, true), Res),
    equal(Res, -1.1421... -1.1420).

test(pt17) :-
    interval(pt(-0.5, 5, false, false), Res),
    equal(Res, 0.6808...0.6809).

test(pt18) :-
    interval(pt(-0.5, 5, false, true), Res),
    equal(Res, -0.3845... -0.3844).

test(pt19) :-
    interval(pt(-0.5... -0.2, 5, true, false), Res),
    equal(Res, 0.3191...0.4247).

test(pt20) :-
    interval(pt(-0.5... -0.2, 5, true, true), Res),
    equal(Res, -1.1421... -0.8564).

test(pt21) :-
    interval(pt(-0.5... -0.2, 5, false, false), Res),
    equal(Res, 0.5753...0.6809).

test(pt22) :-
    interval(pt(-0.5... -0.2, 5, false, true), Res),
    equal(Res, -0.5529... -0.3844).

test(pt23) :-
    interval(pt(-0.5, 5...6, true, false), Res),
    equal(Res, 0.3174...0.3192).

test(pt24) :-
    interval(pt(-0.5, 5...6, true, true), Res),
    equal(Res, -1.1475... -1.1420).

test(pt25) :-
    interval(pt(-0.5, 5...6, false, false), Res),
    equal(Res, 0.6808...0.6826).

test(pt26) :-
    interval(pt(-0.5, 5...6, false, true), Res),
    equal(Res, -0.3845... -0.3819).

test(pt27) :-
    interval(pt(-0.5... -0.2, 5...6, true, false), Res),
    equal(Res, 0.3174...0.4247).

test(pt28) :-
    interval(pt(-0.5... -0.2, 5...6, true, true), Res),
    equal(Res, -1.1475... -0.8564).

test(pt29) :-
    interval(pt(0.5...1, 2...5, false, false), Res),
    equal(Res, 0.1816...0.3334).

test(pt30) :-
    interval(pt(0.5...1, 2...5, false, true), Res),
    equal(Res, -1.7060... -1.0986).

test(pt31) :-
    interval(pt(-0.5...0.2, 2...5, true, false), Res),
    equal(Res, 0.4999...0.6809).

test(pt32) :-
    interval(pt(-0.5...0.2, 2...5, true, true), Res),
    equal(Res, -0.6932... -0.3844).

test(pt33) :-
    interval(pt(-0.5...0.2, 2...5, false, false), Res),
    equal(Res, 0.3191...0.5001).

test(pt34) :-
    interval(pt(-0.5...0.2, 2...5, false, true), Res),
    equal(Res, -1.1421... -0.6931).

test(qt1) :-
    interval(qt(0.4, 5), Res),
    equal(Res, -0.2672... -0.2671).

test(qt2) :-
    interval(qt(0.4...0.7, 5), Res),
    equal(Res, -0.2672...0.5595).

test(qt3) :-
    interval(qt(0.4, 5...6), Res),
    equal(Res, -0.2672... -0.2648).

test(qt4) :-
    interval(qt(0.4...0.7, 2...5), Res),
    equal(Res, -0.2887...0.5595).

test(qt5) :-
    interval(qt(0.4, 5, true), Res),
    equal(Res, -0.2672... -0.2671).

test(qt6) :-
    interval(qt(0.6, 5, true), Res),
    equal(Res, 0.2671... 0.2672).

test(qt6) :-
    interval(qt(0.4, 5, false), Res),
    equal(Res, 0.2671...0.2672).

test(qt7) :-
    interval(qt(0.6, 5, false), Res),
    equal(Res, -0.2672... -0.2671).

test(qt8) :-
    interval(qt(0.3...0.4, 5, true), Res),
    equal(Res, -0.5595... -0.2671).

test(qt9) :-
    interval(qt(0.6...0.7, 5, true), Res),
    equal(Res, 0.2671... 0.5595).

test(qt10) :-
    interval(qt(0.4...0.7, 5, true), Res),
    equal(Res, -0.2672...0.5595).

test(qt11) :-
    interval(qt(0.3...0.4, 5, false), Res),
    equal(Res, 0.2671...0.5595).

test(qt12) :-
    interval(qt(0.6...0.7, 5, false), Res),
    equal(Res, -0.5595... -0.2671).

test(qt13) :-
    interval(qt(0.4...0.7, 5, false), Res),
    equal(Res, -0.5595...0.2672).

test(qt14) :-
    interval(qt(0.4, 5...6, true), Res),
    equal(Res, -0.2672... -0.2648).

test(qt15) :-
    interval(qt(0.6, 5...6, true), Res),
    equal(Res, 0.2648...0.2672).

test(qt16) :-
    interval(qt(0.4, 5...6, false), Res),
    equal(Res, 0.2648...0.2672).

test(qt17) :-
    interval(qt(0.6, 5...6, false), Res),
    equal(Res, -0.2672... -0.2648).

test(qt18) :-
    interval(qt(0.4...0.7, 2...5, true), Res),
    equal(Res, -0.2887...0.5595).

test(qt19) :-
    interval(qt(0.4...0.7, 2...5, false), Res),
    equal(Res, -0.5595...0.2887).

test(qt20) :-
    interval(qt(0.4, 5, 0), Res),
    equal(Res, -0.2672... -0.2671).

test(qt21) :-
    interval(qt(0.4, 5, 1), Res),
    equal(Res, 0.7779...0.7780).

test(qt22) :-
    interval(qt(0.6...0.7, 5, 0), Res),
    equal(Res, 0.2671... 0.5595).

test(qt23) :-
    interval(qt(0.6...0.7, 5, 1), Res),
    equal(Res, 1.3424...1.6759).

test(qt24) :-
    interval(qt(0.4, 5...6, 0), Res),
    equal(Res, -0.2672... -0.2648).

test(qt25) :-
    interval(qt(0.4, 5...6, 1), Res),
    equal(Res, 0.7725...0.7780).

test(qt26) :-
    interval(qt(0.4, 5, 1...2), Res),
    equal(Res, 0.7779...1.8051).

test(qt27) :-
    interval(qt(0.4...0.7, 2...5, 0), Res),
    equal(Res, -0.2887...0.5595).

test(qt28) :-
    interval(qt(0.4...0.7, 2...5, 1), Res),
    equal(Res, 0.7779...1.9633).

test(qt29) :-
    interval(qt(0.4...0.7, 5, 1...2), Res),
    equal(Res, 0.7779...2.8529).

test(qt30) :-
    interval(qt(0.4, 5...6, 1...2), Res),
    equal(Res, 0.7725...1.8051).

test(qt31) :-
    interval(qt(0.4...0.6, 5...6, 1...2), Res),
    equal(Res, 0.7725...2.4516).

test(qt32) :-
    interval(qt(0.4, 5, true, false), Res),
    equal(Res, -0.2672... -0.2671).

test(qt33) :-
    interval(qt(-0.91629, 5, true, true), Res),
    equal(Res, -0.2672... -0.2671).

test(qt34) :-
    interval(qt(0.6, 5, true, false), Res),
    equal(Res, 0.2671... 0.2672).

test(qt35) :-
    interval(qt(-0.51082, 5, true, true), Res),
    equal(Res, 0.2671... 0.2672).

test(qt36) :-
    interval(qt(0.4, 5, false, false), Res),
    equal(Res, 0.2671...0.2672).

test(qt37) :-
    interval(qt(-0.91629, 5, false, true), Res),
    equal(Res, 0.2671...0.2672).

test(qt38) :-
    interval(qt(0.6, 5, false, false), Res),
    equal(Res, -0.2672... -0.2671).

test(qt39) :-
    interval(qt(-0.51082, 5, false, true), Res),
    equal(Res, -0.2672... -0.2671).

test(qt40) :-
    interval(qt(0.3...0.4, 5, true, false), Res),
    equal(Res, -0.5595... -0.2671).

test(qt41) :-
    interval(qt(-1.20397... -0.91629, 5, true, true), Res),
    equal(Res, -0.5595... -0.2671).

test(qt42) :-
    interval(qt(0.6...0.7, 5, true, false), Res),
    equal(Res, 0.2671... 0.5595).

test(qt43) :-
    interval(qt(-0.51082... -0.35667, 5, true, true), Res),
    equal(Res, 0.2671... 0.5595).

test(qt44) :-
    interval(qt(0.4...0.7, 5, true, false), Res),
    equal(Res, -0.2672...0.5595).

test(qt45) :-
    interval(qt(-0.91629... -0.35667, 5, true, true), Res),
    equal(Res, -0.2672...0.5595).

test(qt46) :-
    interval(qt(0.3...0.4, 5, false, false), Res),
    equal(Res, 0.2671...0.5595).

test(qt47) :-
    interval(qt(-1.20397... -0.91629, 5, false, true), Res),
    equal(Res, 0.2671...0.5595).

test(qt48) :-
    interval(qt(0.6...0.7, 5, false, false), Res),
    equal(Res, -0.5595... -0.2671).

test(qt49) :-
    interval(qt(-0.51082... -0.35667, 5, false, true), Res),
    equal(Res, -0.5595... -0.2671).

test(qt50) :-
    interval(qt(0.4...0.7, 5, false, false), Res),
    equal(Res, -0.5595...0.2672).

test(qt51) :-
    interval(qt(-0.91629... -0.35667, 5, false, true), Res),
    equal(Res, -0.5595...0.2672).

test(qt52) :-
    interval(qt(0.4, 5...6, true, false), Res),
    equal(Res, -0.2672... -0.2648).

test(qt53) :-
    interval(qt(-0.91629, 5...6, true, true), Res),
    equal(Res, -0.2672... -0.2648).

test(qt54) :-
    interval(qt(0.6, 5...6, true, false), Res),
    equal(Res, 0.2648...0.2672).

test(qt55) :-
    interval(qt(-0.51082, 5...6, true, true), Res),
    equal(Res, 0.2648...0.2672).

test(qt56) :-
    interval(qt(0.4, 5...6, false, false), Res),
    equal(Res, 0.2648...0.2672).

test(qt57) :-
    interval(qt(-0.91629, 5...6, false, true), Res),
    equal(Res, 0.2648...0.2672).

test(qt58) :-
    interval(qt(0.6, 5...6, false, false), Res),
    equal(Res, -0.2672... -0.2648).

test(qt59) :-
    interval(qt(-0.51082, 5...6, false, true), Res),
    equal(Res, -0.2672... -0.2648).

test(qt60) :-
    interval(qt(0.4...0.7, 2...5, true, false), Res),
    equal(Res, -0.2887...0.5595).

test(qt61) :-
    interval(qt(-0.91629... -0.35667, 2...5, true, true), Res),
    equal(Res, -0.2887...0.5595).

test(qt62) :-
    interval(qt(0.4...0.7, 2...5, false, false), Res),
    equal(Res, -0.5595...0.2887).

test(qt63) :-
    interval(qt(-0.91629... -0.35667, 2...5, false, true), Res),
    equal(Res, -0.5595...0.2887).

test(dt1) :-
    interval(dt(-0.5, 5), Res),
    equal(Res, 0.3279...0.3280).

test(dt2) :-
    interval(dt(-0.5... -0.4, 5), Res),
    equal(Res, 0.3279...0.3454).

test(dt3) :-
    interval(dt(0.4...0.5, 5), Res),
    equal(Res, 0.3279...0.3454).

test(dt4) :-
    interval(dt(-0.4... 0.5, 5), Res),
    equal(Res, 0.3279...0.3797).

test(dt5) :-
    interval(dt(-0.5, 5...6), Res),
    equal(Res, 0.3279...0.3318).

test(dt6) :-
    interval(dt(0.4, 5...6), Res),
    equal(Res, 0.3453...0.3491).

test(dt7) :-
    interval(dt(-0.5... -0.4, 5...6), Res),
    equal(Res, 0.3279...0.3491).

test(dt8) :-
    interval(dt(0.4...0.5, 5...6), Res),
    equal(Res, 0.3279...0.3491).

test(dt9) :-
    interval(dt(-0.4... 0.5, 5...6), Res),
    equal(Res, 0.3279...0.3828).

test(dt10) :-
    interval(dt(-0.5, 5, false), Res),
    equal(Res, 0.3279...0.3280).

test(dt10) :-
    interval(dt(-0.5, 5, true), Res),
    equal(Res, -1.1150... -1.1149).

test(dt11) :-
    interval(dt(-0.5... -0.4, 5, false), Res),
    equal(Res, 0.3279...0.3454).

test(dt12) :-
    interval(dt(-0.5... -0.4, 5, true), Res),
    equal(Res, -1.1150... -1.0631).

test(dt13) :-
    interval(dt(0.4...0.5, 5, false), Res),
    equal(Res, 0.3279...0.3454).

test(dt14) :-
    interval(dt(0.4...0.5, 5, true), Res),
    equal(Res, -1.1150... -1.0631).

test(dt15) :-
    interval(dt(-0.4... 0.5, 5, false), Res),
    equal(Res, 0.3279...0.3797).

test(dt16) :-
    interval(dt(-0.4... 0.5, 5, true), Res),
    equal(Res, -1.1150... -0.9686).

test(dt17) :-
    interval(dt(-0.5, 5...6, false), Res),
    equal(Res, 0.3279...0.3318).

test(dt18) :-
    interval(dt(-0.5, 5...6, true), Res),
    equal(Res, -1.1150... -1.1032).

test(dt19) :-
    interval(dt(0.4, 5...6, false), Res),
    equal(Res, 0.3453...0.3491).

test(dt20) :-
    interval(dt(0.4, 5...6, true), Res),
    equal(Res, -1.0632... -1.0525).

test(dt21) :-
    interval(dt(-0.5... -0.4, 5...6, false), Res),
    equal(Res, 0.3279...0.3491).

test(dt22) :-
    interval(dt(-0.5... -0.4, 5...6, true), Res),
    equal(Res, -1.1150... -1.0525).

test(dt23) :-
    interval(dt(0.4...0.5, 5...6, false), Res),
    equal(Res, 0.3279...0.3491).

test(dt24) :-
    interval(dt(0.4...0.5, 5...6, true), Res),
    equal(Res, -1.1150... -1.0525).

test(dt25) :-
    interval(dt(-0.4... 0.5, 5...6, false), Res),
    equal(Res, 0.3279...0.3828).

test(dt26) :-
    interval(dt(-0.4... 0.5, 5...6, true), Res),
    equal(Res, -1.1150... -0.9604).

test(dt27) :-
    interval(dt(2, 10...15), Res),
    equal(Res, 0.0592...0.0612).

test(dt28) :-
    interval(dt(0.5, 5, 0), Res),
    equal(Res, 0.3279...0.3280).

test(dt29) :-
    interval(dt(0.5, 5, 1), Res),
    equal(Res, 0.3360...0.3361).

test(dt30) :-
    interval(dt(0.4...0.5, 5, 0), Res),
    equal(Res, 0.3279...0.3454).

test(dt31) :-
    interval(dt(0.4...0.5, 5, 1), Res),
    equal(Res, 0.3192...0.3361).

test(dt32) :-
    interval(dt(1.5...2, 5, 1), Res),
    equal(Res, 0.2114...0.3021).

test(dt33) :-
    interval(dt(0.5...1.5, 5, 1), Res),
    equal(Res, 0.3020...0.3653).

test(dt34) :-
    interval(dt(0.5, 5...6, 0), Res),
    equal(Res, 0.3279...0.3318).

test(dt35) :-
    interval(dt(0.5, 5...6, 1), Res),
    equal(Res, 0.3360...0.3387).

test(dt36) :-
    interval(dt(1.5, 5...6, 1), Res),
    equal(Res, 0.3020...0.3093).

test(dt37) :-
    interval(dt(0.4...0.5, 5...6, 0), Res),
    equal(Res, 0.3279...0.3491).

test(dt38) :-
    interval(dt(0.4...0.5, 5...6, 1), Res),
    equal(Res, 0.3192...0.3387).

test(dt39) :-
    interval(dt(1.5...2, 5...6, 1), Res),
    equal(Res, 0.2114...0.3093).

test(dt40) :-
    interval(dt(0.5...1.5, 5...6, 1), Res),
    equal(Res, 0.3020...0.3702).

test(dt41) :-
    interval(dt(0.4...0.5, 5, 1...2), Res),
    equal(Res, 0.1102...0.3361).

test(dt42) :-
    interval(dt(2...3, 5, 1...2), Res),
    equal(Res, 0.0812...0.3223).

test(dt43) :-
    interval(dt(0.5...2, 5, 1...2), Res),
    equal(Res, 0.1297...0.3653).

test(dt44) :-
    interval(dt(0.5, 5...6, 1...2), Res),
    equal(Res, 0.1297...0.3387).

test(dt45) :-
    interval(dt(1.5, 5...6, 1...2), Res),
    equal(Res, 0.3020...0.3239).

test(dt46) :-
    interval(dt(0.4...0.5, 5...6, 1...2), Res),
    equal(Res, 0.1102...0.3387).

test(dt47) :-
    interval(dt(1.5...2, 5...6, 1...2), Res),
    equal(Res, 0.2114...0.3385).

test(dt48) :-
    interval(dt(0.5...1.5, 5...6, 1...2), Res),
    equal(Res, 0.1297...0.3702).

test(dt49) :-
    interval(dt(0.5, 5, 0, false), Res),
    equal(Res, 0.3279...0.3280).

test(dt50) :-
    interval(dt(0.5, 5, 1, false), Res),
    equal(Res, 0.3360...0.3361).

test(dt51) :-
    interval(dt(0.5, 5, 1, true), Res),
    equal(Res, -1.0907... -1.0906).

test(dt52) :-
    interval(dt(0.4...0.5, 5, 0, false), Res),
    equal(Res, 0.3279...0.3454).

test(dt53) :-
    interval(dt(0.4...0.5, 5, 1, false), Res),
    equal(Res, 0.3192...0.3361).

test(dt54) :-
    interval(dt(0.4...0.5, 5, 1, true), Res),
    equal(Res, -1.1419... -1.0906).

test(dt55) :-
    interval(dt(1.5...2, 5, 1, false), Res),
    equal(Res, 0.2114...0.3021).

test(dt56) :-
    interval(dt(1.5...2, 5, 1, true), Res),
    equal(Res, -1.5540... -1.1970).

test(dt57) :-
    interval(dt(0.5...1.5, 5, 1, false), Res),
    equal(Res, 0.3020...0.3653).

test(dt58) :-
    interval(dt(0.5...1.5, 5, 1, true), Res),
    equal(Res, -1.1971... -1.0071).

test(dt59) :-
    interval(dt(0.5, 5...6, 0, false), Res),
    equal(Res, 0.3279...0.3318).

test(dt60) :-
    interval(dt(0.5, 5...6, 1, false), Res),
    equal(Res, 0.3360...0.3387).

test(dt61) :-
    interval(dt(0.5, 5...6, 1, true), Res),
    equal(Res, -1.0907... -1.0828).

test(dt62) :-
    interval(dt(1.5, 5...6, 1, false), Res),
    equal(Res, 0.3020...0.3093).

test(dt63) :-
    interval(dt(1.5, 5...6, 1, true), Res),
    equal(Res, -1.1971... -1.1737).

test(dt64) :-
    interval(dt(0.4...0.5, 5...6, 0, false), Res),
    equal(Res, 0.3279...0.3491).

test(dt65) :-
    interval(dt(0.4...0.5, 5...6, 1, false), Res),
    equal(Res, 0.3192...0.3387).

test(dt66) :-
    interval(dt(0.4...0.5, 5...6, 1, true), Res),
    equal(Res, -1.1419... -1.0828).

test(dt67) :-
    interval(dt(1.5...2, 5...6, 1, false), Res),
    equal(Res, 0.2114...0.3093).

test(dt68) :-
    interval(dt(1.5...2, 5...6, 1, true), Res),
    equal(Res, -1.5540... -1.1737).

test(dt69) :-
    interval(dt(0.5...1.5, 5...6, 1, false), Res),
    equal(Res, 0.3020...0.3702).

test(dt70) :-
    interval(dt(0.5...1.5, 5...6, 1, true), Res),
    equal(Res, -1.1971... -0.9937).

test(dt71) :-
    interval(dt(0.4...0.5, 5, 1...2, false), Res),
    equal(Res, 0.1102...0.3361).

test(dt72) :-
    interval(dt(0.4...0.5, 5, 1...2, true), Res),
    equal(Res, -2.2052... -1.0906).

test(dt73) :-
    interval(dt(2...3, 5, 1...2, false), Res),
    equal(Res, 0.0812...0.3223).

test(dt74) :-
    interval(dt(2...3, 5, 1...2, true), Res),
    equal(Res, -2.5108... -1.1323).

test(dt75) :-
    interval(dt(0.5...2, 5, 1...2, false), Res),
    equal(Res, 0.1297...0.3653).

test(dt76) :-
    interval(dt(0.5...2, 5, 1...2, true), Res),
    equal(Res, -2.0425... -1.0071).

test(dt77) :-
    interval(dt(0.5, 5...6, 1...2, false), Res),
    equal(Res, 0.1297...0.3387).

test(dt78) :-
    interval(dt(0.5, 5...6, 1...2, true), Res),
    equal(Res, -2.0425... -1.0828).

test(dt79) :-
    interval(dt(1.5, 5...6, 1...2, false), Res),
    equal(Res, 0.3020...0.3239).

test(dt80) :-
    interval(dt(1.5, 5...6, 1...2, true), Res),
    equal(Res, -1.1971... -1.1273).

test(dt81) :-
    interval(dt(0.4...0.5, 5...6, 1...2, false), Res),
    equal(Res, 0.1102...0.3387).

test(dt82) :-
    interval(dt(0.4...0.5, 5...6, 1...2, true), Res),
    equal(Res, -2.2052... -1.0828).

test(dt83) :-
    interval(dt(1.5...2, 5...6, 1...2, false), Res),
    equal(Res, 0.2114...0.3385).

test(dt84) :-
    interval(dt(1.5...2, 5...6, 1...2, true), Res),
    equal(Res, -1.5540... -1.0835).

test(dt85) :-
    interval(dt(0.5...1.5, 5...6, 1...2, false), Res),
    equal(Res, 0.1297...0.3702).

test(dt86) :-
    interval(dt(0.5...1.5, 5...6, 1...2, true), Res),
    equal(Res, -2.0425... -0.9937).
    
:- end_tests(t).

:- begin_tests(chisq).

test(pchisq1) :-
    interval(pchisq(0.3, 5), Res),
    equal(Res, 0.0023...0.0024).

test(pchisq2) :-
    interval(pchisq(0.3...0.4, 5), Res),
    equal(Res, 0.0023...0.0047).

test(pchisq3) :-
    interval(pchisq(0.3, 5...6), Res),
    equal(Res, 0.0005...0.0024).

test(pchisq4) :-
    interval(pchisq(0.3...0.4, 5...6), Res),
    equal(Res, 0.0005...0.0047).

test(pchisq5) :-
    interval(pchisq(0.3, 5, true), Res),
    equal(Res, 0.0023...0.0024).

test(pchisq6) :-
    interval(pchisq(0.3, 5, false), Res),
    equal(Res, 0.9976...0.9977).

test(pchisq7) :-
    interval(pchisq(0.3...0.4, 5, true), Res),
    equal(Res, 0.0023...0.0047).

test(pchisq8) :-
    interval(pchisq(0.3...0.4, 5, false), Res),
    equal(Res, 0.9953...0.9977).

test(pchisq9) :-
    interval(pchisq(0.3, 5...6, true), Res),
    equal(Res, 0.0005...0.0024).

test(pchisq10) :-
    interval(pchisq(0.3, 5...6, false), Res),
    equal(Res, 0.9976...0.9995).

test(pchisq11) :-
    interval(pchisq(0.3...0.4, 5...6, true), Res),
    equal(Res, 0.0005...0.0047).

test(pchisq12) :-
    interval(pchisq(0.3...0.4, 5...6, false), Res),
    equal(Res, 0.9953...0.9995).

test(pchisq13) :-
    interval(pchisq(0.3, 5, true, false), Res),
    equal(Res, 0.0023...0.0024).

test(pchisq14) :-
    interval(pchisq(0.3, 5, true, true), Res),
    equal(Res, -6.0505... -6.0504).

test(pchisq15) :-
    interval(pchisq(0.3, 5, false, false), Res),
    equal(Res, 0.9976...0.9977).

test(pchisq16) :-
    interval(pchisq(0.3, 5, false, true), Res),
    equal(Res, -0.0024... -0.0023).

test(pchisq17) :-
    interval(pchisq(0.3...0.4, 5, true, false), Res),
    equal(Res, 0.0023...0.0047).

test(pchisq18) :-
    interval(pchisq(0.3...0.4, 5, true, true), Res),
    equal(Res, -6.0505... -5.3665).

test(pchisq19) :-
    interval(pchisq(0.3...0.4, 5, false, false), Res),
    equal(Res, 0.9953...0.9977).

test(pchisq20) :-
    interval(pchisq(0.3...0.4, 5, false, true), Res),
    equal(Res, -0.0047... -0.0023).

test(pchisq21) :-
    interval(pchisq(0.3, 5...6, true, false), Res),
    equal(Res, 0.0005...0.0024).

test(pchisq22) :-
    interval(pchisq(0.3, 5...6, true, true), Res),
    equal(Res, -7.5952... -6.0504).

test(pchisq23) :-
    interval(pchisq(0.3, 5...6, false, false), Res),
    equal(Res, 0.9976...0.9995).

test(pchisq24) :-
    interval(pchisq(0.3, 5...6, false, true), Res),
    equal(Res, -0.0024... -0.0005).

test(pchisq25) :-
    interval(pchisq(0.3...0.4, 5...6, true, false), Res),
    equal(Res, 0.0005...0.0047).

test(pchisq26) :-
    interval(pchisq(0.3...0.4, 5...6, true, true), Res),
    equal(Res, -7.5952... -5.3665).

test(pchisq27) :-
    interval(pchisq(0.3...0.4, 5...6, false, false), Res),
    equal(Res, 0.9953...0.9995).

test(pchisq28) :-
    interval(pchisq(0.3...0.4, 5...6, false, true), Res),
    equal(Res, -0.0047... -0.0005).

test(qchisq1) :-
    interval(qchisq(0.3, 5), Res),
    equal(Res, 2.9999...3).

test(qchisq2) :-
    interval(qchisq(0.3...0.4, 5), Res),
    equal(Res, 2.9999...3.6555).

test(qchisq3) :-
    interval(qchisq(0.3, 5...6), Res),
    equal(Res, 2.9999...3.8276).

test(qchisq4) :-
    interval(qchisq(0.3...0.4, 5...6), Res),
    equal(Res, 2.9999...4.5702).

test(qchisq5) :-
    interval(qchisq(0.3, 5, true), Res),
    equal(Res, 2.9999...3).

test(qchisq6) :-
    interval(qchisq(0.3, 5, false), Res),
    equal(Res, 6.0644...6.0645).

test(qchisq7) :-
    interval(qchisq(0.3...0.4, 5, true), Res),
    equal(Res, 2.9999...3.6555).
    
test(qchisq8) :-
    interval(qchisq(0.3...0.4, 5, false), Res),
    equal(Res, 5.1318...6.0645).

test(qchisq9) :-
    interval(qchisq(0.3, 5...6, true), Res),
    equal(Res, 2.9999...3.8276).

test(qchisq10) :-
    interval(qchisq(0.3, 5...6, false), Res),
    equal(Res, 6.0644...7.2312).

test(qchisq11) :-
    interval(qchisq(0.3...0.4, 5...6, true), Res),
    equal(Res, 2.9999...4.5702).

test(qchisq12) :-
    interval(qchisq(0.3...0.4, 5...6, false), Res),
    equal(Res, 5.1318...7.2312).

test(qchisq13) :-
    interval(qchisq(0.3, 5, true, false), Res),
    equal(Res, 2.9999...3).

test(qchisq14) :-
    interval(qchisq(-1.20397, 5, true, true), Res),
    equal(Res, 2.9999...3).

test(qchisq15) :-
    interval(qchisq(0.3, 5, false, false), Res),
    equal(Res, 6.0644...6.0645).

test(qchisq16) :-
    interval(qchisq(-1.20397, 5, false, true), Res),
    equal(Res, 6.0644...6.0645).

test(qchisq17) :-
    interval(qchisq(0.3...0.4, 5, true, false), Res),
    equal(Res, 2.9999...3.6555).

test(qchisq18) :-
    interval(qchisq(-1.20397... -0.91629, 5, true, true), Res),
    equal(Res, 2.9999...3.6556).

test(qchisq19) :-
    interval(qchisq(0.3...0.4, 5, false, false), Res),
    equal(Res, 5.1318...6.0645).

test(qchisq20) :-
    interval(qchisq(-1.20397... -0.91629, 5, false, true), Res),
    equal(Res, 5.1318...6.0645).

test(qchisq21) :-
    interval(qchisq(0.3, 5...6, true, false), Res),
    equal(Res, 2.9999...3.8276).

test(qchisq22) :-
    interval(qchisq(-1.20397, 5...6, true, true), Res),
    equal(Res, 2.9999...3.8276).

test(qchisq23) :-
    interval(qchisq(0.3, 5...6, false, false), Res),
    equal(Res, 6.0644...7.2312).

test(qchisq24) :-
    interval(qchisq(-1.20397, 5...6, false, true), Res),
    equal(Res, 6.0644...7.2312).

test(qchisq25) :-
    interval(qchisq(0.3...0.4, 5...6, true, false), Res),
    equal(Res, 2.9999...4.5702).

test(qchisq26) :-
    interval(qchisq(-1.20397... -0.91629, 5...6, true, true), Res),
    equal(Res, 2.9999...4.5702).

test(qchisq27) :-
    interval(qchisq(0.3...0.4, 5...6, false, false), Res),
    equal(Res, 5.1318...7.2312).

test(qchisq28) :-
    interval(qchisq(-1.20397... -0.91629, 5...6, false, true), Res),
    equal(Res, 5.1318...7.2312).

test(dchisq1) :-
    interval(dchisq(0.3, 2), Res),
    equal(Res, 0.4303...0.4304).

test(dchisq2) :-
    interval(dchisq(1.4, 4), Res),
    equal(Res, 0.1738...0.1739).

test(dchisq3) :-
    interval(dchisq(2.1, 4), Res),
    equal(Res, 0.1837...0.1838).

test(dchisq4) :-
    interval(dchisq(0.3...0.4, 2), Res),
    equal(Res, 0.4093...0.4304).

test(dchisq5) :-
    interval(dchisq(0.8...0.9, 3), Res),
    equal(Res, 0.2391...0.2414).

test(dchisq6) :-
    interval(dchisq(1.3...1.4, 3), Res),
    equal(Res, 0.2344...0.2375).

test(dchisq7) :-
    interval(dchisq(0.9...1.1, 3), Res),
    equal(Res, 0.2413...0.2420).

test(dchisq8) :-
    interval(dchisq(0.3, 1...2), Res),
    equal(Res, 0.4303...0.627).

test(dchisq9) :-
    interval(dchisq(1.1, 2...3), Res),
    equal(Res, 0.2414...0.2885).

test(dchisq10) :-
    interval(dchisq(0.3, 3...4), Res),
    equal(Res, 0.0645...0.1881).

test(dchisq11) :-
    interval(dchisq(2.2, 3...4), Res),
    equal(Res, 0.1830...0.1970).

test(dchisq12) :-
    interval(dchisq(0.9...1.1, 1...2), Res),
    equal(Res, 0.2194...0.3189).

test(dchisq13) :-
    interval(dchisq(2...3, 2...3), Res),
    equal(Res, 0.1115...0.2076).

test(dchisq14) :-
    interval(dchisq(0.9...1.1, 2...3), Res),
    equal(Res, 0.2413...0.3189).

test(dchisq15) :-
    interval(dchisq(0.8...0.9, 3...4), Res),
    equal(Res, 0.1340...0.2414).

test(dchisq16) :-
    interval(dchisq(2.1...2.2, 3...4), Res),
    equal(Res, 0.1830...0.2024).

test(dchisq17) :-
    interval(dchisq(0.9...1.1, 3...4), Res),
    equal(Res, 0.1434...0.2420).

test(dchisq18) :-
    interval(dchisq(0.3, 2, false), Res),
    equal(Res, 0.4303...0.4304).

test(dchisq19) :-
    interval(dchisq(0.3, 2, true), Res),
    equal(Res, -0.8432... -0.8431).

test(dchisq20) :-
    interval(dchisq(1.4, 4, false), Res),
    equal(Res, 0.1738...0.1739).

test(dchisq21) :-
    interval(dchisq(1.4, 4, true), Res),
    equal(Res, -1.7499... -1.7498).

test(dchisq22) :-
    interval(dchisq(2.1, 4, false), Res),
    equal(Res, 0.1837...0.1838).

test(dchisq23) :-
    interval(dchisq(2.1, 4, true), Res),
    equal(Res, -1.6944... -1.6943).

test(dchisq24) :-
    interval(dchisq(0.3...0.4, 2, false), Res),
    equal(Res, 0.4093...0.4304).

test(dchisq25) :-
    interval(dchisq(0.3...0.4, 2, true), Res),
    equal(Res, -0.8932... -0.8431).

test(dchisq26) :-
    interval(dchisq(0.8...0.9, 3, false), Res),
    equal(Res, 0.2391...0.2414).

test(dchisq27) :-
    interval(dchisq(0.8...0.9, 3, true), Res),
    equal(Res, -1.4306... -1.4216).

test(dchisq28) :-
    interval(dchisq(1.3...1.4, 3, false), Res),
    equal(Res, 0.2344...0.2375).

test(dchisq29) :-
    interval(dchisq(1.3...1.4, 3, true), Res),
    equal(Res, -1.4508... -1.4377).

test(dchisq30) :-
    interval(dchisq(0.9...1.1, 3, false), Res),
    equal(Res, 0.2413...0.2420).

test(dchisq31) :-
    interval(dchisq(0.9...1.1, 3, true), Res),
    equal(Res, -1.4217... -1.4189).

test(dchisq32) :-
    interval(dchisq(0.3, 1...2, false), Res),
    equal(Res, 0.4303...0.627).

test(dchisq33) :-
    interval(dchisq(0.3, 1...2, true), Res),
    equal(Res, -0.8432... -0.4669).

test(dchisq34) :-
    interval(dchisq(1.1, 2...3, false), Res),
    equal(Res, 0.2414...0.2885).

test(dchisq35) :-
    interval(dchisq(1.1, 2...3, true), Res),
    equal(Res, -1.4213... -1.2431).

test(dchisq36) :-
    interval(dchisq(0.3, 3...4, false), Res),
    equal(Res, 0.0645...0.1881).

test(dchisq37) :-
    interval(dchisq(0.3, 3...4, true), Res),
    equal(Res, -2.7403... -1.6709).

test(dchisq38) :-
    interval(dchisq(2.2, 3...4, false), Res),
    equal(Res, 0.1830...0.1970).

test(dchisq39) :-
    interval(dchisq(2.2, 3...4, true), Res),
    equal(Res, -1.6979... -1.6247).

test(dchisq40) :-
    interval(dchisq(0.9...1.1, 1...2, false), Res),
    equal(Res, 0.2194...0.3189).

test(dchisq41) :-
    interval(dchisq(0.9...1.1, 1...2, true), Res),
    equal(Res, -1.5166... -1.1431).

test(dchisq42) :-
    interval(dchisq(2...3, 2...3, false), Res),
    equal(Res, 0.1115...0.2076).

test(dchisq43) :-
    interval(dchisq(2...3, 2...3, true), Res),
    equal(Res, -2.1932... -1.5723).

test(dchisq44) :-
    interval(dchisq(0.9...1.1, 2...3, false), Res),
    equal(Res, 0.2413...0.3189).

test(dchisq45) :-
    interval(dchisq(0.8...0.9, 3...4, true), Res),
    equal(Res, -2.0095... -1.4216).

test(dchisq46) :-
    interval(dchisq(2.1...2.2, 3...4, false), Res),
    equal(Res, 0.1830...0.2024).

test(dchisq47) :-
    interval(dchisq(2.1...2.2, 3...4, true), Res),
    equal(Res, -1.6979... -1.5979).

test(dchisq48) :-
    interval(dchisq(0.9...1.1, 3...4, false), Res),
    equal(Res, 0.1434...0.2420).

test(dchisq49) :-
    interval(dchisq(0.9...1.1, 3...4, true), Res),
    equal(Res, -1.9417... -1.4189).

:- end_tests(chisq).

% Helper predicate to check equality
equal(Res0, Res) :-
    interval(round(Res0, 4), Res).