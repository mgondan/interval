:- module(test_cleaning, [test_cleaning/0, op(150, xfx, ...)]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- consult(['../inst/prolog/lib/cleaning_clp.pl',
            '../inst/prolog/lib/cleaning.pl']).

test_cleaning :-
    run_tests([clean, unclean]).

:- begin_tests(clean).

test(clean1a) :-
    clean(5, B),
    B = number(5).

test(clean1b) :-
    clean(5.1, B),
    B = number(5.1).

test(clean2) :-
    clean([], B),
    B = [].

test(clean3) :-
    clean(true, B),
    B = bool(true).

test(clean4) :-
    clean(false, B),
    B = bool(false).

test(clean5) :-
    clean("a", B),
    B = string("a").

test(clean6) :-
    clean(a, B),
    B = atomic(a).

test(clean7) :-
    clean(1...2, B),
    B = 1...2.

test(clean8) :-
    clean(1 + (1...2 / 1.1 - a), B),
    B = number(1) + (1...2 / number(1.1) - atomic(a)).

test(clean9) :-
    clean([1, a, "a", 1...2, 1 + 2], B),
    B = [number(1), atomic(a), string("a"), 1...2, number(1) + number(2)].

test(clean10) :-
    clean(pi, B),
    B = number(pi).

test(clean11) :-
    clean(e, B),
    B = number(e).

:- end_tests(clean).

:- begin_tests(unclean).

test(unclean1) :-
    clean(A, number(5)),
    A = 5.

test(unclean1b) :-
    clean(A, number(5.1)),
    A = 5.1.

test(unclean2) :-
    clean(A, []),
    A = [].

test(unclean3) :-
    clean(A, bool(true)),
    A = true.

test(unclean4) :-
    clean(A, bool(false)),
    A = false.

test(unclean5) :-
    clean(A, string("a")),
    A = "a".

test(unclean6) :-
    clean(A, atomic(a)),
    A = a.

test(unclean7) :-
    clean(A, 1...2),
    A = 1...2.

test(unclean8) :-
    clean(A, number(1) + (1...2 / number(1.1) - atomic(a))),
    A = 1 + (1...2 / 1.1 - a).

test(unclean9) :-
    clean(A, [number(1), atomic(a), string("a"), 1...2, number(1) + number(2)]),
    A = [1, a, "a", 1...2, 1 + 2].

test(unclean10) :-
    clpBNR:{B > 1},
    clean(A, number(B)),
    A = 1.0000000000000002...1.0Inf.

test(unclean11) :-
    clpBNR:{B < 1},
    clean(A, number(B)),
    A = -1.0Inf...0.9999999999999999.

test(unclean12) :-
    clpBNR:{_ is 5 + B},
    clean(A, number(B)),
    A = -1.0Inf...1.0Inf.

test(unclean13) :-
    clpBNR:{B is 1.0 / 3.0},
    clean(A, number(B)),
    A = 0.3333333333333333.

test(unclean14) :-
    clpBNR:{B is 1 / 3},
    clean(A, number(B)),
    A = 0.3333333333333333.

test(unclean15) :-
    clpBNR:{L is 1.0 / 3.0},
    U is 1,
    clean(A, L...U),
    A = 0.3333333333333333...1.

test(unclean16) :-
    L is 0.2,
    clpBNR:{U is 2.0 / 3.0},
    clean(A, L...U),
    A = 0.2...0.6666666666666667.

test(unclean17) :-
    clpBNR:{L is 1.0 / 3.0},
    clpBNR:{U is 2.0 / 3.0},
    clean(A, L...U),
    A = 0.3333333333333333...0.6666666666666667.

:- end_tests(unclean).