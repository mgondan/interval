:- module(test_utility, [test_utility/0]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../prolog/rint.pl').

test_utility :-
    run_tests([arrange_args, eval_min_max, optimize]).

:- begin_tests(arrange_args).

test(arrange_args1) :-
  Args = [1],
  rint:arrange_args(Args, Res),
  Res = [##(1)].

test(arrange_args2) :-
  Args = [1...2],
  rint:arrange_args(Args, Res),
  Res = [##(2, 1)].

test(arrange_args3) :-
  Args = [1...2, 3...4],
  rint:arrange_args(Args, Res),
  Res = [##(2, 2, 1, 1), ##(4, 3, 4, 3)].

test(arrange_args4) :-
  Args = [1...2, 3...4, 5...6],
  rint:arrange_args(Args, Res),
  Res = [##(2, 2, 2, 2, 1, 1, 1, 1), ##(4, 4, 3, 3, 4, 4, 3, 3), ##(6, 5, 6, 5, 6, 5, 6, 5)].

test(arrange_args5) :-
  Args = [true],
  rint:arrange_args(Args, Res),
  Res = [true].

test(arrange_args6) :-
  Args = [1...2, false],
  rint:arrange_args(Args, Res),
  Res = [##(2, 1), false].

test(arrange_args7) :-
  Args = [1...2, true, 3...4],
  rint:arrange_args(Args, Res),
  Res = [##(2, 2, 1, 1), true, ##(4, 3, 4, 3)].

test(arrange_args8) :-
  Args = [1...2, log=true, 3...4],
  rint:arrange_args(Args, Res),
  Res = [##(2, 2, 1, 1), log=true, ##(4, 3, 4, 3)].

test(arrange_args9) :-
  Args = [1...1, log=true, 3...4],
  rint:arrange_args(Args, Res),
  Res = [##(1, 1), log=true, ##(4, 3)].

test(arrange_args10) :-
  Args = [1...1, log=true, 3...3],
  rint:arrange_args(Args, Res),
  Res = [##(1), log=true, ##(3)].

:- end_tests(arrange_args).

:- begin_tests(eval_min_max).

test(eval_min_max1) :-
  rint:eval_min_max(dt, [1...2, 10...20], Res),
  rint:interval(r(dt(1, 10)), X1),
  rint:interval(r(dt(1, 20)), X2),
  rint:interval(r(dt(2, 10)), X3),
  rint:interval(r(dt(2, 20)), X4),
  rint:min_list([X1, X2, X3, X4], L),
  rint:max_list([X1, X2, X3, X4], U),
  equal(Res, L...U).

test(eval_min_max2) :-
  rint:eval_min_max(dt, [1...2, 10...20], [mode([0.9, 0.1])], Res),
  rint:interval(r(dt(1, 10)), X1),
  rint:interval(r(dt(1, 20)), X2),
  rint:interval(r(dt(2, 10)), X3),
  rint:interval(r(dt(2, 20)), X4),
  rint:min_list([X1, X2, X3, X4, 0.9, 0.1], L),
  rint:max_list([X1, X2, X3, X4, 0.9, 0.1], U),
  equal(Res, L...U).

:- end_tests(eval_min_max).

:- begin_tests(optimize).

test(optimize1) :-
  rint:optimize_(dt, [1...2, 2, 4], true, [Actual]),
  rint:eval(r(optimize(dt, ##(1, 2), 2, 4, maximum=true)), [_, _-Expected]),
  equal(Actual, Expected).

test(optimize2) :-
  rint:optimize_(dt, [1...2, 2...3, 4], true, Actual),
  rint:eval(r(optimize(dt, ##(1, 2), 2, 4, maximum=true)), [_, _-Expected1]),
  rint:eval(r(optimize(dt, ##(1, 2), 3, 4, maximum=true)), [_, _-Expected2]),
  equal(Actual, [Expected1, Expected2]).

test(optimize3) :-
  rint:optimize_(dt, [1...2, 2...3, 4...5], true, Actual),
  rint:eval(r(optimize(dt, ##(1, 2), 2, 4, maximum=true)), [_, _-Expected1]),
  rint:eval(r(optimize(dt, ##(1, 2), 2, 5, maximum=true)), [_, _-Expected2]),
  rint:eval(r(optimize(dt, ##(1, 2), 3, 4, maximum=true)), [_, _-Expected3]),
  rint:eval(r(optimize(dt, ##(1, 2), 3, 5, maximum=true)), [_, _-Expected4]),
  equal(Actual, [Expected1, Expected2, Expected3, Expected4]).

:- end_tests(optimize).


% Helper predicate to check equality
equal([A | B], [C | D]) :-
  msort([A | B], List1),
  msort([C | D], List2),
  !, maplist(equal, List1, List2, _).

equal(Res0, Res1) :-
  interval(round(Res0, 4), Res),
  interval(round(Res1, 4), Res).

equal(A, B, Res) :-
  equal(A, B),
  !, Res = true. 
