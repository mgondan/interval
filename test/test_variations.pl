:- module(test_variations, [test_variations/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../inst/prolog/lib/variations.pl').

test_variations :-
    run_tests([variations]).

:- begin_tests(variations).

test(test1, [fail]) :-
    variations(1, [..., number], _Variations).

test(test2) :-
  variations(1, [number], Actual),
  Expected = [[number]],
  equal(Actual, Expected).

test(test3) :-
  variations(2, [number], Actual),
  Expected = [[number, number]],
  equal(Actual, Expected).

test(test4) :-
  variations(2, [number, ...], Actual),
  Expected = [[..., ...], [..., number], [number, ...], [number, number]],
  equal(Actual, Expected).
    
test(test5) :-
  variations(3, [number, ...], Actual),
  Expected = [[..., ..., ...], [..., ..., number], [..., number, number], 
                [number, number, number], [number, ..., ...], [number, number, ...],
                [number, ..., number], [..., number, ...]],
  equal(Actual, Expected).

test(test6, [fail]) :-
  variations(3, [number, ...], [_, _], _Actual).

test(test7) :-
  variations(2, [number, ...], [_, _], Actual),
  Expected = [[..., ...], [..., number], [number, ...], [number, number]],
  equal(Actual, Expected). 

test(test8) :-
  variations(2, [number, ...], [_, number], Actual),
  Expected = [[..., number], [number, number]],
  equal(Actual, Expected). 

test(test9) :-
  variations(2, [number, ...], [_, string], Actual),
  Expected = [[..., string], [number, string]],
  equal(Actual, Expected). 

test(test10) :-
  variations(2, [number, ...], [_, [string, number]], Actual),
  Expected = [[..., string], [number, string], [..., number], [number, number]],
  equal(Actual, Expected). 

test(test11) :-
  variations(3, [number, ...], [_, [string, number], [bool(false), bool(true)]], Actual),
  Expected = [[..., string, bool(false)], [number, string, bool(false)], [..., number, bool(false)], [number, number, bool(false)],
              [..., string, bool(true)], [number, string, bool(true)], [..., number, bool(true)], [number, number, bool(true)]],
  equal(Actual, Expected). 

test(test12) :-
  variations(3, [number], [_, [string, number], [bool(false), bool(true)]], Actual),
  Expected = [[number, string, bool(false)], [number, number, bool(false)],
              [number, string, bool(true)], [number, number, bool(true)]],
  equal(Actual, Expected). 

test(test13) :-
  variations(3, [], [number, [string, number], [bool(false), bool(true)]], Actual),
  Expected = [[number, string, bool(false)], [number, number, bool(false)],
              [number, string, bool(true)], [number, number, bool(true)]],
  equal(Actual, Expected). 

test(test14) :-
  variations(2, [], [list, [string, number]], Actual),
  Expected = [[_, string], [_, number]],
  equal(Actual, Expected). 

:- end_tests(variations).

equal(A0, B0) :-
  sort(A0, A),
  sort(B0, B),
  ?=(A, B).
  