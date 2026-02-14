:- module(test_cleaning, [test_cleaning/0, op(150, xfx, ...)]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../inst/prolog/lib/cleaning.pl').

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

test(clean12) :-
    clean(number(1), B),
    B = number(1).

test(clean13) :-
    clean(bool(true), B),
    B = bool(true).

test(clean14) :-
    clean(string("abc"), B),
    B = string("abc").

test(clean15) :-
    clean(atomic(a), B),
    B = atomic(a).

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

:- end_tests(unclean).