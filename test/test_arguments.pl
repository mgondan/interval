:- module(test_arguments, [test_arguments/0, op(150, xfx, ...)]).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module('../inst/prolog/lib/arguments.pl').

test_arguments :-
    run_tests([test]).

example(Fun, Signature) :-
    Fun = pbinom,
    Signature = args{1:[q], 2:[size], 3:[prob], 4:['lower.tail', true], 5:['log.p', false]}.

:- begin_tests(test).

test(test1) :-
    example(Fun, Signature),
    UserArgs = [atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5)],
    process_args(Fun, Signature, UserArgs, Call),
    Call = pbinom(atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5), atomic('lower.tail')=bool(true), atomic('log.p')=bool(false)).

test(test2) :-
    example(Fun, Signature),
    UserArgs = [atomic(prob)=number(0.5), atomic(size)=number(20), atomic(q)=number(10)],
    process_args(Fun, Signature, UserArgs, Call),
    Call = pbinom(atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5), atomic('lower.tail')=bool(true), atomic('log.p')=bool(false)).

test(test3) :-
    example(Fun, Signature),
    UserArgs = [number(10), number(20), number(0.5)],
    process_args(Fun, Signature, UserArgs, Call),
    Call = pbinom(atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5), atomic('lower.tail')=bool(true), atomic('log.p')=bool(false)).

test(test4) :-
    example(Fun, Signature),
    UserArgs = [number(10), number(20), number(0.5), atomic('lower.tail')=bool(false)],
    process_args(Fun, Signature, UserArgs, Call),
    Call = pbinom(atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5), atomic('lower.tail')=bool(false), atomic('log.p')=bool(false)).

test(test5) :-
    example(Fun, Signature),
    UserArgs = [number(10), number(20), number(0.5), atomic('lower.tail')=bool(false), atomic('log.p')=bool(true)],
    process_args(Fun, Signature, UserArgs, Call),
    Call = pbinom(atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5), atomic('lower.tail')=bool(false), atomic('log.p')=bool(true)).

test(test6) :-
    example(Fun, Signature),
    UserArgs = [number(10), number(20), number(0.5), bool(false), bool(true)],
    process_args(Fun, Signature, UserArgs, Call),
    Call = pbinom(atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5), atomic('lower.tail')=bool(false), atomic('log.p')=bool(true)).

test(test7) :-
    example(Fun, Signature),
    UserArgs = [number(10), number(20), number(0.5), bool(false), bool(true), number(5)],
    process_args(Fun, Signature, UserArgs, Call),
    Call = pbinom(atomic(q)=number(10), atomic(size)=number(20), atomic(prob)=number(0.5), atomic('lower.tail')=bool(false), atomic('log.p')=bool(true), number(5)).

:- end_tests(test).
