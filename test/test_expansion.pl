:- module(test_expansion, [test_expansion/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../inst/prolog/lib/expansion.pl').

test_expansion :-
    run_tests([macro_clause_3, macro_clause_4, macro_clause_5]).

:- begin_tests(macro_clause_3).

test(test1_1) :-
    macro_clause((*)/2, mixed, Clauses),
    Clauses = [ (interval_(A...B * number(C), Res, _) :-
                  !, interval_(A...B * C...C, Res, _)),
                (interval_(number(A) * C...D, Res, _) :-
                  !, interval_(A...A * C...D, Res, _))
              ].

test(test1_2) :-
    macro_clause(pt/3, mixed, extra(bool(Tail), 3), Clauses),
    Clauses = [ (interval_(pt(A...B, number(C), bool(Tail)), Res, _) :-
                  !, interval_(pt(A...B, C...C, bool(Tail)), Res, _)),
                (interval_(pt(number(A), C...D, bool(Tail)), Res, _) :-
                  !, interval_(pt(A...A, C...D, bool(Tail)), Res, _))
              ].
              
:- end_tests(macro_clause_3).

:- begin_tests(macro_clause_4).

test(test2_1) :-
    macro_clause((+)/2, plus/2, [+, +], Clauses),
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

test(test2_2) :-
    macro_clause((-)/2, minus/2, [+, -], Clauses),
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

test(test2_3) :-
    macro_clause((-)/1, unary_negate/1, [+], Clauses),
    Clauses = [ (interval_(-(A...B), Res, _) :-
                  unary_negate(A, L), 
                  unary_negate(B, U), 
                  !, Res = L...U)
              ].

:- end_tests(macro_clause_4).

:- begin_tests(macro_clause_5).

test(test3_1) :-
    macro_clause(func/3, func_/2, extra(bool(true), 2), [+, +], Clauses),
    Clauses = [ (interval_(func(A...B, bool(true), C...D), Res, _) :-
                  func_(A, C, L), 
                  func_(B, D, U), 
                  !, Res = L...U), 
                (interval_(func(A...B, bool(true), number(C)), Res, _) :-
                  func_(A, C, L), 
                  func_(B, C, U), 
                  !, Res = L...U), 
                (interval_(func(number(A), bool(true), B...C), Res, _) :-
                  func_(A, B, L), 
                  func_(A, C, U), 
                  !, Res = L...U)
              ].

  test(test3_2) :-
    macro_clause(func/3, func_/2, extra(bool(false), 3), [+, -], Clauses),
    Clauses = [ (interval_(func(A...B, C...D, bool(false)), Res, _) :-
                  func_(A, D, L), 
                  func_(B, C, U), 
                  !, Res = L...U), 
                (interval_(func(A...B, number(C), bool(false)), Res, _) :-
                  func_(A, D, L), 
                  func_(B, C, U), 
                  !, Res = L...U), 
                (interval_(func(number(A), B...C, bool(false)), Res, _) :-
                  func_(A, C, L), 
                  func_(A, B, U), 
                  !, Res = L...U)
              ].

:- end_tests(macro_clause_5).