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

eval(Expr1, Expr2, L ... U) :-
    eval(Expr1, L),
    eval(Expr2, U).

% Force atomic to interval
interval_(atomic(A), Res, _Flags),
    Res = L...U
 => L = A,
    U = A.

interval_(atomic(A), Res, _Flags)
 => Res = atomic(A).

interval_(L...U, Res, _Flags)
 => Res = L...U.

% Skip evaluation of arguments
interval_(Expr, Res, Flags),
    compound(Expr),
    compound_name_arguments(Expr, Name, Args),
    int_hook(Name, Mask, Res0, [evaluate(false) | _T]),
    compound_name_arguments(Mask, Fun, Types),
    maplist(instantiate, Types, Args),
    compound_name_arguments(Goal, Fun, Args),
    findall(Res1, call(Goal, Res1, Flags), Sol),
    maplist(instantiate(Res0), Sol)
 => member(Res, Sol).

% Evaluate arguments
interval_(Expr, Res, Flags),
    compound(Expr),
    compound_name_arguments(Expr, Name, Args),
    maplist(interval__(Flags), Args, Args1),
    compound_name_arguments(Expr1, Name, Args1)
 => interval2_(Expr1, Res, Flags).

interval__(Flags, A, Res) :-
    interval_(A, Res, Flags).

instantiate(A, Res), 
    A = atomic
 => Res = atomic(_).

instantiate(A, Res), 
    Res = atomic(_)
 => A = atomic.

instantiate(A, Res), 
    A = ...
 => Res = _..._.

instantiate(A, Res), 
    Res = _..._
 => A = (...).

instantiate(A, B),
    var(A)
 => A = B.

% Find int_hook
interval2_(Expr, Res, Flags),
    compound(Expr),
    compound_name_arguments(Expr, Name, Args),
    maplist(instantiate, Types, Args),
    int_hook(Name, Mask, Res0, _Opt),
    compound_name_arguments(Mask, Fun, Types),
    compound_name_arguments(Goal, Fun, Args),
    findall(Res1, call(Goal, Res1, Flags), Sol),
    maplist(instantiate(Res0), Sol)
 => member(Res, Sol).

% Special case: multiplication ([*, *], commutative)
interval2_(Expr, Res, _Flags),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, **)
 => compound_name_arguments(Expr, Name, Args),
    findall(R, both(Name, Args, R), Bounds),
    min_list(Bounds, L),
    max_list(Bounds, U),
    Res = L...U.

% General case 
interval2_(Expr, Res, _Flags),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Dir)
 => compound_name_arguments(Expr, Name, Args),
    findall(R, lower(Dir, Name, Args, R), Lower),
    min_list(Lower, L),
    findall(R, upper(Dir, Name, Args, R), Upper),
    max_list(Upper, U),  
    return(L, U, Res).

%
% Default case
%
interval2_(_, _, _Flags)
 => fail.

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
