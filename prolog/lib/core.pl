%
% Arithmetic functions for single numbers
%
% Define hooks for R functions etc.
%
eval(Expr, Res),
    eval_hook(Expr, R)
 => Res = R.

eval(X, Res)
 => Res is X.

% For convenience
eval(Expr1, Expr2, L ... U) :-
    eval(Expr1, L),
    eval(Expr2, U).


interval_(atomic(A), Res, _Flags),
    Res = L...U
 => L = A,
    U = A.

interval_(atomic(A), Res, _Flags)
 => Res = atomic(A).

interval_(L...U, Res, _Flags)
 => Res = L...U.

interval_(Expr, Res, Flags),
    compound(Expr),
    compound_name_arguments(Expr, Name, Args),
    int_hook(Name, Mask, Res0, Opt),
    option(evaluate(true), Opt, true),
    instantiate(Res0, Res),
    compound_name_arguments(Mask, Fun, Args1),
    maplist(instantiate, Args1, Args2),
    maplist(interval__(Flags), Args, Args2)
 => compound_name_arguments(Goal, Fun, Args2),
    call(Goal, Res, Flags).

interval__(Flags, A, Res) :-
    interval_(A, Res, Flags).

instantiate(A, Res), 
    A = atomic
 => Res = atomic(_).

instantiate(A, Res), 
    A = ...
 => Res = _..._.

instantiate(A, Res),
    var(A)
 => Res = A.

% Skipping evaluation of arguments
interval_(Expr, Res, Flags),
    compound(Expr),
    compound_name_arguments(Expr, Name, Args),
    int_hook(Name, Mask, Res0, Opt),
    option(evaluate(false), Opt, true),
    instantiate(Res0, Res),
    compound_name_arguments(Mask, Fun, Args1),
    maplist(instantiate, Args1, Args2),
    maplist(instantiate_, Args, Args2)
 => compound_name_arguments(Goal, Fun, Args2),
    call(Goal, Res, Flags).

instantiate_(atomic(A), Res),
    Res = atomic(_)
 => Res = atomic(A).

instantiate_(atomic(A), Res),
    Res = _..._
 => Res = A...A.

instantiate_(L...U, Res),
    Res = _..._
 => Res = L...U.

instantiate_(ci(A, B), Res),
    Res = ci(_, _)
 => Res = ci(A, B).

instantiate_(A, Res),
    var(Res)
 => Res = A.

% special case: multiplication ([*, *], commutative)
interval_(Expr, Res, Flags),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, **)
 => compound_name_arguments(Expr, Name, Args),
    maplist(interval__(Flags), Args, Args1),
    findall(R, both(Name, Args1, R), Bounds),
    min_list(Bounds, L),
    max_list(Bounds, U),
    Res = L...U.

% general case
interval_(Expr, Res, Flags),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Dir)
 => compound_name_arguments(Expr, Name, Args),
    maplist(interval__(Flags), Args, Args1),
    findall(R, lower(Dir, Name, Args1, R), Lower),
    min_list(Lower, L),
    findall(R, upper(Dir, Name, Args1, R), Upper),
    max_list(Upper, U),
    Res = L...U.

%
% Default case
%
interval_(_, _, _Flags)
 => fail.

lower(Dir, Name, Args, Res) :-
    maplist(lower, Dir, Args, Lower),
    Expr =.. [Name | Lower],
    eval(Expr, Res).

upper(Dir, Name, Args, Res) :-
    maplist(upper, Dir, Args, Upper),
    Expr =.. [Name | Upper],
    eval(Expr, Res).

both(Name, Args, Res) :-
    maplist(lower(*), Args, Lower),
    Expr =.. [Name | Lower],
    eval(Expr, Res).

% Obtain lower and upper bounds
lower(+, A..._, L)
 => L = A.

lower(-, _...A, L)
 => L = A.

lower(*, A...B, L)
 => L = A ; L = B.

lower(_, atomic(A), L)
 => L = A.

lower(_, A, L),
    atomic(A)
 => L = A.

upper(+, _...B, U)
 => U = B.

upper(-, A..._, U)
 => U = A.

upper(*, A...B, U)
 => U = A ; U = B.

upper(_, atomic(A), U)
 => U = A.

upper(_, A, U),
    atomic(A)
 => U = A.