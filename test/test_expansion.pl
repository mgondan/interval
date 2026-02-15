:- module(test_expansion, [test_expansion/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../inst/prolog/lib/expansion.pl').

test_expansion :-
    run_tests([macro_clause]).

:- begin_tests(macro_clause).

test(test1) :-
    macro_clause(n, Clauses),
    Clauses = [ (interval_(atomic(n), Res, _) :-
                  eval(n, Res0),
                  !, clean(Res0, Res))
              ].

test(test2) :-
    macro_clause(n, [hook(r_session:r_topic)], Clauses),
    Clauses = [ (interval_(atomic(n), Res, _) :-
                  eval(hook(r_session:r_topic, n), Res0),
                  !, clean(Res0, Res))
              ].

test(test3) :-
    macro_clause(n, [prefix(rint)], Clauses),
    Clauses = [ (interval_(atomic(n), Res, _) :-
                  rint:eval(n, Res0),
                  !, rint:clean(Res0, Res))
              ].

test(test4) :-
    macro_clause(n, [hook(r_session:r_topic), prefix(rint)], Clauses),
    Clauses = [ (interval_(atomic(n), Res, _) :-
                  rint:eval(hook(r_session:r_topic, n), Res0),
                  !, rint:clean(Res0, Res))
              ].

test(test5) :-
    macro_clause(n, [names([size])], Clauses),
    Clauses = [ (interval_(atomic(size)=atomic(n), Res, _) :-
                  eval(n, Res0),
                  !, clean(Res0, Res))
              ].

test(test6) :-
    macro_clause(n, [names([size]), hook(r_session:r_topic)], Clauses),
    Clauses = [ (interval_(atomic(size)=atomic(n), Res, _) :-
                  eval(hook(r_session:r_topic, n), Res0),
                  !, clean(Res0, Res))
              ].

test(test7) :-
    macro_clause(n, [prefix(rint), names([size])], Clauses),
    Clauses = [ (interval_(atomic(size)=atomic(n), Res, _) :-
                  rint:eval(n, Res0),
                  !, rint:clean(Res0, Res))
              ].

test(test8) :-
    macro_clause(n, [hook(r_session:r_topic), names([size]), prefix(rint)], Clauses),
    Clauses = [ (interval_(atomic(size)=atomic(n), Res, _) :-
                  rint:eval(hook(r_session:r_topic, n), Res0),
                  !, rint:clean(Res0, Res))
              ].

test(test9) :-
    macro_clause((*)/2, interval_, [], Clauses),
    Clauses = [ (interval_(A...B * number(C), Res, _) :-
                  !, interval_(A...B * C...C, Res, _)),
                (interval_(number(A) * C...D, Res, _) :-
                  !, interval_(A...A * C...D, Res, _))
              ].

test(test10) :-
    macro_clause(pt/3, interval_, [], [pattern([_, _, bool])], Clauses),
    Clauses = [ (interval_(pt(A...B, number(C), bool(Tail)), Res, _) :-
                  !, interval_(pt(A...B, C...C, bool(Tail)), Res, _)),
                (interval_(pt(number(A), C...D, bool(Tail)), Res, _) :-
                  !, interval_(pt(A...A, C...D, bool(Tail)), Res, _))
              ].


test(test11) :-
    macro_clause(pt/2, interval_, [], [prefix(interval)], Clauses),
    Clauses = [ (interval_(pt(A...B, number(C)), Res, _) :-
                  !, interval:interval_(pt(A...B, C...C), Res, _)),
                (interval_(pt(number(A), C...D), Res, _) :-
                  !, interval:interval_(pt(A...A, C...D), Res, _))
              ].

test(test12) :-
    macro_clause(pt/3, interval_, [], [prefix(interval), pattern([_, _, bool])], Clauses),
    Clauses = [ (interval_(pt(A...B, number(C), bool(Tail)), Res, _) :-
                  !, interval:interval_(pt(A...B, C...C, bool(Tail)), Res, _)),
                (interval_(pt(number(A), C...D, bool(Tail)), Res, _) :-
                  !, interval:interval_(pt(A...A, C...D, bool(Tail)), Res, _))
              ].

test(test13) :-
    macro_clause((*)/2, interval_, [], [names([left, right])], Clauses),
    Clauses = [ (interval_((atomic(left)=A...B)*(atomic(right)=number(C)), Res, _) :-
                  !, interval_((atomic(left)=A...B) * (atomic(right)=C...C), Res, _)),
                (interval_((atomic(left)=number(A))*(atomic(right)=C...D), Res, _) :-
                  !, interval_((atomic(left)=A...A) * (atomic(right)=C...D), Res, _))
              ].

test(test14) :-
    macro_clause(pt/3, interval_, [], [prefix(interval), pattern([_, _, bool]), names([q, df, 'lower.tail'])], Clauses),
    Clauses = [ (interval_(pt(atomic(q)=A...B, atomic(df)=number(C), atomic('lower.tail')=bool(Tail)), Res, _) :-
                  !, interval:interval_(pt(atomic(q)=A...B, atomic(df)=C...C, atomic('lower.tail')=bool(Tail)), Res, _)),
                (interval_(pt(atomic(q)=number(A), atomic(df)=C...D, atomic('lower.tail')=bool(Tail)), Res, _) :-
                  !, interval:interval_(pt(atomic(q)=A...A, atomic(df)=C...D, atomic('lower.tail')=bool(Tail)), Res, _))
              ].

test(test15) :-
    macro_clause((+)/2, all, [+, +], Clauses),
    Clauses = [ (interval_(A...B + C...D, Res, _):-
                  eval(A + C, L),
                  eval(B + D, U),
                  !, Res = L...U),
                (interval_(A...B + number(C), Res, _):-
                  eval(A + C, L),
                  eval(B + C, U),
                  !, Res = L...U),
                (interval_(number(A) + B...C, Res, _):-
                  eval(A + B, L),
                  eval(A + C, U),
                  !, Res = L...U),
                (interval_(number(A) + number(B), Res, _) :-
                  eval(A + B, Res0),
                  !, Res = number(Res0))
              ].

test(test16) :-
    macro_clause(choose/2, all, [+, +], [hook(r_session:r_topic)], Clauses),
    Clauses = [ (interval_(choose(A...B, C...D), Res, _):-
                  eval(hook(r_session:r_topic, choose(A, C)), L),
                  eval(hook(r_session:r_topic, choose(B, D)), U),
                  !, Res = L...U),
                (interval_(choose(A...B, number(C)), Res, _):-
                  eval(hook(r_session:r_topic, choose(A, C)), L),
                  eval(hook(r_session:r_topic, choose(B, C)), U),
                  !, Res = L...U),
                (interval_(choose(number(A), B...C), Res, _):-
                  eval(hook(r_session:r_topic, choose(A, B)), L),
                  eval(hook(r_session:r_topic, choose(A, C)), U),
                  !, Res = L...U),
                (interval_(choose(number(A), number(B)), Res, _) :-
                  eval(hook(r_session:r_topic, choose(A, B)), Res0),
                  !, Res = number(Res0))
              ].

test(test17) :-
    macro_clause(choose/2, all, [+, +], [prefix(rint)], Clauses),
    Clauses = [ (interval_(choose(A...B, C...D), Res, _):-
                  rint:eval(choose(A, C), L),
                  rint:eval(choose(B, D), U),
                  !, Res = L...U),
                (interval_(choose(A...B, number(C)), Res, _):-
                  rint:eval(choose(A, C), L),
                  rint:eval(choose(B, C), U),
                  !, Res = L...U),
                (interval_(choose(number(A), B...C), Res, _):-
                  rint:eval(choose(A, B), L),
                  rint:eval(choose(A, C), U),
                  !, Res = L...U),
                (interval_(choose(number(A), number(B)), Res, _) :-
                  rint:eval(choose(A, B), Res0),
                  !, Res = number(Res0))
              ].

test(test18) :-
    macro_clause(func/2, all, [/,+], [pattern([bool(true), _])], Clauses),
    Clauses = [ (interval_(func(bool(true), A...B), Res, _):-
                  eval(func(true, A), L),
                  eval(func(true, B), U),
                  !, Res = L...U),
                (interval_(func(bool(true), number(A)), Res, _):-
                  eval(func(true, A), Res0),
                  !, Res = number(Res0))
              ].

test(test19) :-
    macro_clause((+)/2, all, [+, +], [names([left, right])], Clauses),
    Clauses = [ (interval_((atomic(left)=A...B)+(atomic(right)=C...D), Res, _):-
                  eval(A + C, L),
                  eval(B + D, U),
                  !, Res = L...U),
                (interval_((atomic(left)=A...B)+(atomic(right)=number(C)), Res, _):-
                  eval(A + C, L),
                  eval(B + C, U),
                  !, Res = L...U),
                (interval_((atomic(left)=number(A))+(atomic(right)=B...C), Res, _):-
                  eval(A + B, L),
                  eval(A + C, U),
                  !, Res = L...U),
                (interval_((atomic(left)=number(A))+(atomic(right)=number(B)), Res, _) :-
                  eval(A + B, Res0),
                  !, Res = number(Res0))
              ].

test(test20) :-
    macro_clause(func/2, all, [/,+], [pattern([bool(true), _]), prefix(rint), hook(r_session:r_topic)], Clauses),
    Clauses = [ (interval_(func(bool(true), A...B), Res, _):-
                  rint:eval(hook(r_session:r_topic, func(true, A)), L),
                  rint:eval(hook(r_session:r_topic, func(true, B)), U),
                  !, Res = L...U),
                (interval_(func(bool(true), number(A)), Res, _):-
                  rint:eval(hook(r_session:r_topic, func(true, A)), Res0),
                  !, Res = number(Res0))
              ].

test(test21) :-
    macro_clause(var_pool/4, all, [+, /, +, /], [pattern([_, number, _, number])], Clauses),
    Clauses = [ (interval_(var_pool(A...B, number(C), D...E, number(F)), Res, _):-
                  eval(var_pool(A, C, D, F), L),
                  eval(var_pool(B, C, E, F), U),
                  !, Res = L...U),
                (interval_(var_pool(A...B, number(C), number(D), number(F)), Res, _):-
                  eval(var_pool(A, C, D, F), L),
                  eval(var_pool(B, C, D, F), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), D...E, number(F)), Res, _):-
                  eval(var_pool(A, C, D, F), L),
                  eval(var_pool(A, C, E, F), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), number(D), number(F)), Res, _):-
                  eval(var_pool(A, C, D, F), Res0),
                  !, Res = number(Res0))
              ].

test(test22) :-
    macro_clause(var_pool/4, all, [+, /, +, /], [hook(r), pattern([_, number, _, number])], Clauses),
    Clauses = [ (interval_(var_pool(A...B, number(C), D...E, number(F)), Res, _):-
                  eval(r(var_pool(A, C, D, F)), L),
                  eval(r(var_pool(B, C, E, F)), U),
                  !, Res = L...U),
                (interval_(var_pool(A...B, number(C), number(D), number(F)), Res, _):-
                  eval(r(var_pool(A, C, D, F)), L),
                  eval(r(var_pool(B, C, D, F)), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), D...E, number(F)), Res, _):-
                  eval(r(var_pool(A, C, D, F)), L),
                  eval(r(var_pool(A, C, E, F)), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), number(D), number(F)), Res, _):-
                  eval(r(var_pool(A, C, D, F)), Res0),
                  !, Res = number(Res0))
              ].

test(test23) :-
    macro_clause(var_pool/4, all, [+, /, +, /], [prefix(rint), pattern([_, number, _, number])], Clauses),
    Clauses = [ (interval_(var_pool(A...B, number(C), D...E, number(F)), Res, _):-
                  rint:eval(var_pool(A, C, D, F), L),
                  rint:eval(var_pool(B, C, E, F), U),
                  !, Res = L...U),
                (interval_(var_pool(A...B, number(C), number(D), number(F)), Res, _):-
                  rint:eval(var_pool(A, C, D, F), L),
                  rint:eval(var_pool(B, C, D, F), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), D...E, number(F)), Res, _):-
                  rint:eval(var_pool(A, C, D, F), L),
                  rint:eval(var_pool(A, C, E, F), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), number(D), number(F)), Res, _):-
                  rint:eval(var_pool(A, C, D, F), Res0),
                  !, Res = number(Res0))
              ].

test(test24) :-
    macro_clause(var_pool/4, all, [+, /, +, /], [prefix(rint), hook(r), pattern([_, number, _, number])], Clauses),
    Clauses = [ (interval_(var_pool(A...B, number(C), D...E, number(F)), Res, _):-
                  rint:eval(r(var_pool(A, C, D, F)), L),
                  rint:eval(r(var_pool(B, C, E, F)), U),
                  !, Res = L...U),
                (interval_(var_pool(A...B, number(C), number(D), number(F)), Res, _):-
                  rint:eval(r(var_pool(A, C, D, F)), L),
                  rint:eval(r(var_pool(B, C, D, F)), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), D...E, number(F)), Res, _):-
                  rint:eval(r(var_pool(A, C, D, F)), L),
                  rint:eval(r(var_pool(A, C, E, F)), U),
                  !, Res = L...U),
                (interval_(var_pool(number(A), number(C), number(D), number(F)), Res, _):-
                  rint:eval(r(var_pool(A, C, D, F)), Res0),
                  !, Res = number(Res0))
              ].

test(test25) :-
    macro_clause((-)/1, unary_negate, [+], Clauses),
    Clauses = [ (interval_(-(A...B), Res, _) :-
                  unary_negate(A, L), 
                  unary_negate(B, U), 
                  !, Res = L...U)
              ].

test(test26) :-
    macro_clause((-)/1, all, [+], Clauses),
    Clauses = [ (interval_(-(A...B), Res, _) :-
                  eval(-A, L), 
                  eval(-B, U), 
                  !, Res = L...U),
                (interval_(-A, Res, _) :-
                  eval(-A, Res0), 
                  !, Res = number(Res0))
              ].

test(test27) :-
    macro_clause((-)/1, unary_negate, [+], [prefix(rint)], Clauses),
    Clauses = [ (interval_(-(A...B), Res, _) :-
                  rint:unary_negate(A, L), 
                  rint:unary_negate(B, U), 
                  !, Res = L...U)
              ].

test(test28) :-
    macro_clause((-)/1, all, [+], [prefix(rint)], Clauses),
    Clauses = [ (interval_(-(A...B), Res, _) :-
                  rint:eval(-A, L), 
                  rint:eval(-B, U), 
                  !, Res = L...U),
                (interval_(-A, Res, _) :-
                  rint:eval(-A, Res0), 
                  !, Res = number(Res0))
              ].

test(test29) :-
    macro_clause(func/2, unary_negate, [/, +], [pattern([bool(false), _])], Clauses),
    Clauses = [ (interval_(func(bool(false), A...B), Res, _) :-
                  unary_negate(false, A, L), 
                  unary_negate(false, B, U), 
                  !, Res = L...U)
              ].

test(test30) :-
    macro_clause(func/2, unary_negate, [/, +], [pattern([bool(true), _]), prefix(rint)], Clauses),
    Clauses = [ (interval_(func(bool(true), A...B), Res, _) :-
                  rint:unary_negate(true, A, L), 
                  rint:unary_negate(true, B, U), 
                  !, Res = L...U)
              ].

test(test31) :-
    macro_clause((+)/2, plus, [+, +], Clauses),
    Clauses = [ (interval_(A...B + C...D, Res, _) :-
                  plus(A, C, L), 
                  plus(B, D, U), 
                  !, Res = L...U), 
                (interval_(A...B + number(C), Res, _) :-
                  plus(A, C, L), 
                  plus(B, C, U), 
                  !, Res = L...U), 
                (interval_(number(A)+ B...C, Res, _) :-
                  plus(A, B, L), 
                  plus(A, C, U), 
                  !, Res = L...U)
              ].

test(test32) :-
    macro_clause((-)/2, minus, [+, -], Clauses),
    Clauses = [ (interval_(A...B - C...D, Res, _) :-
                  minus(A, D, L), 
                  minus(B, C, U), 
                  !, Res = L...U), 
                (interval_(A...B - number(C), Res, _) :-
                  minus(A, C, L), 
                  minus(B, C, U), 
                  !, Res = L...U), 
                (interval_(number(A) - B...C, Res, _) :-
                  minus(A, C, L), 
                  minus(A, B, U), 
                  !, Res = L...U)
              ].

test(test32) :-
    macro_clause((-)/2, minus, [+, -], [names([left, right])], Clauses),
    Clauses = [ (interval_((atomic(left)=A...B) - (atomic(right)=C...D), Res, _) :-
                  minus(A, D, L), 
                  minus(B, C, U), 
                  !, Res = L...U), 
                (interval_((atomic(left)=A...B) - (atomic(right)=number(C)), Res, _) :-
                  minus(A, C, L), 
                  minus(B, C, U), 
                  !, Res = L...U), 
                (interval_((atomic(left)=number(A)) - (atomic(right)=B...C), Res, _) :-
                  minus(A, C, L), 
                  minus(A, B, U), 
                  !, Res = L...U)
              ].

test(test33) :-
    macro_clause(func/3, func_, [+,/,+], [pattern([_, bool(true), _])], Clauses),
    Clauses = [ (interval_(func(A...B, bool(true), C...D), Res, _) :-
                  func_(A, true, C, L), 
                  func_(B, true, D, U), 
                  !, Res = L...U), 
                (interval_(func(A...B, bool(true), number(C)), Res, _) :-
                  func_(A, true, C, L), 
                  func_(B, true, C, U), 
                  !, Res = L...U), 
                (interval_(func(number(A), bool(true), B...C), Res, _) :-
                  func_(A, true, B, L), 
                  func_(A, true, C, U), 
                  !, Res = L...U)
              ].

test(test34) :-
    macro_clause((-)/1, unary_negate, [+], [prefix(rint)], Clauses),
    Clauses = [ (interval_(-(A...B), Res, _) :-
                  rint:unary_negate(A, L), 
                  rint:unary_negate(B, U), 
                  !, Res = L...U)
              ].

test(test35) :-
  macro_clause(func/3, func_, [+, -, /], [pattern([_, _, bool(false)]), prefix(interval)], Clauses),
  Clauses = [ (interval_(func(A...B, C...D, bool(false)), Res, _) :-
                interval:func_(A, D, false, L), 
                interval:func_(B, C, false, U), 
                !, Res = L...U), 
              (interval_(func(A...B, number(C), bool(false)), Res, _) :-
                interval:func_(A, D, false, L), 
                interval:func_(B, C, false, U), 
                !, Res = L...U), 
              (interval_(func(number(A), B...C, bool(false)), Res, _) :-
                interval:func_(A, C, false, L), 
                interval:func_(A, B, false, U), 
                !, Res = L...U)
            ].

test(test36) :-
  macro_clause(func/3, func_, [+, -, /], [pattern([_, _, [bool(false), bool(true)]]), prefix(interval)], Clauses),
  Clauses = [ (interval_(func(A...B, C...D, bool(false)), Res, _) :-
                interval:func_(A, D, false, L), 
                interval:func_(B, C, false, U), 
                !, Res = L...U), 
              (interval_(func(A...B, C...D, bool(true)), Res, _) :-
                interval:func_(A, D, true, L), 
                interval:func_(B, C, true, U), 
                !, Res = L...U), 
              (interval_(func(A...B, number(C), bool(false)), Res, _) :-
                interval:func_(A, D, false, L), 
                interval:func_(B, C, false, U), 
                !, Res = L...U), 
              (interval_(func(A...B, number(C), bool(true)), Res, _) :-
                interval:func_(A, D, true, L), 
                interval:func_(B, C, true, U), 
                !, Res = L...U), 
              (interval_(func(number(A), B...C, bool(false)), Res, _) :-
                interval:func_(A, C, false, L), 
                interval:func_(A, B, false, U), 
                !, Res = L...U),
              (interval_(func(number(A), B...C, bool(true)), Res, _) :-
                interval:func_(A, C, true, L), 
                interval:func_(A, B, true, U), 
                !, Res = L...U)
            ].

test(test37) :-
  macro_clause(func/3, func_, [+, -, /], [pattern([_, _, [bool(false), bool(true)]]), prefix(interval), names([a, b, c])], Clauses),
  Clauses = [ (interval_(func(atomic(a)=A...B, atomic(b)=C...D, atomic(c)=bool(false)), Res, _) :-
                interval:func_(A, D, false, L), 
                interval:func_(B, C, false, U), 
                !, Res = L...U), 
              (interval_(func(atomic(a)=A...B, atomic(b)=C...D, atomic(c)=bool(true)), Res, _) :-
                interval:func_(A, D, true, L), 
                interval:func_(B, C, true, U), 
                !, Res = L...U), 
              (interval_(func(atomic(a)=A...B, atomic(b)=number(C), atomic(c)=bool(false)), Res, _) :-
                interval:func_(A, D, false, L), 
                interval:func_(B, C, false, U), 
                !, Res = L...U), 
              (interval_(func(atomic(a)=A...B, atomic(b)=number(C), atomic(c)=bool(true)), Res, _) :-
                interval:func_(A, D, true, L), 
                interval:func_(B, C, true, U), 
                !, Res = L...U), 
              (interval_(func(atomic(a)=number(A), atomic(b)=B...C, atomic(c)=bool(false)), Res, _) :-
                interval:func_(A, C, false, L), 
                interval:func_(A, B, false, U), 
                !, Res = L...U),
              (interval_(func(atomic(a)=number(A), atomic(b)=B...C, atomic(c)=bool(true)), Res, _) :-
                interval:func_(A, C, true, L), 
                interval:func_(A, B, true, U), 
                !, Res = L...U)
            ].

test(test38) :-
  macro_clause(ancova_ci/7, all, [/, /, /, /, /, /, /], [pattern([string, list, list, list, list, list, string])], Clauses),
  Clauses = [(interval_(ancova_ci(string(A), B, C, D, E, F, string(G)), H, _):-
                eval(ancova_ci(A, B, C, D, E, F, G), I), !, H=number(I))
            ].

:- end_tests(macro_clause).
