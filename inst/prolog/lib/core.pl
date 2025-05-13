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

% External definitions of interval_/3 
interval_(A, Res, Flags),
    interval_hook(A, Res1, Flags)
 => Res = Res1.

% Force atomic to interval
interval_(atomic(A), Res, _Flags),
    Res = L...U
 => L = A,
    U = A.

interval_(atomic(A), Res, _Flags)
 => Res = atomic(A).

interval_(L...U, Res, _Flags)
 => Res = L...U.

interval_([], Res, _Flags)
 => Res = [].

interval_([H | T], Res, Flags)
 => maplist(interval__(Flags), [H | T], Res).

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

instantiate(atomic, atomic(_)).
instantiate(..., _..._).
instantiate(A, Res) :-
    var(A),
    A = Res.

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
    findall(R, both_(Name, Args, R), Bounds),
    min_list(Bounds, L),
    max_list(Bounds, U),
    Res = L...U.

% General case 
interval2_(Expr, Res, _Flags),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    mono(Name/Arity, Dir)
 => compound_name_arguments(Expr, Name, Args),
    findall(R, lower_(Dir, Name, Args, R), Lower),
    min_list(Lower, L),
    findall(R, upper_(Dir, Name, Args, R), Upper),
    max_list(Upper, U),  
    return(L, U, Res).

%
% Default case
%
interval2_(_, _, _Flags)
 => fail.

interval_(_, _, _Flags)
 => fail.

lower_(Dir, Name, Args, Res) :-
    maplist(lower_, Dir, Args, Lower),
    Expr =.. [Name | Lower],
    eval(Expr, Res).

upper_(Dir, Name, Args, Res) :-
    maplist(upper_, Dir, Args, Upper),
    Expr =.. [Name | Upper],
    eval(Expr, Res).

both_(Name, Args, Res) :-
    maplist(lower_(*), Args, Lower),
    Expr =.. [Name | Lower],
    eval(Expr, Res).

% Obtain lower and upper bounds
lower_(+, A..._, L)
 => L = A.

lower_(-, _...A, L)
 => L = A.

lower_(*, A...B, L)
 => L = A ; L = B.

lower_(_, atomic(A), L)
 => L = A.

lower_(_, [H | T], L)
 => unwrap([H | T], L).

lower_(_, A, L)
 => L = A.

upper_(+, _...B, U)
 => U = B.

upper_(-, A..._, U)
 => U = A.

upper_(*, A...B, U)
 => U = A ; U = B.

upper_(_, atomic(A), U)
 => U = A.

upper_(_, [H | T], U)
 => unwrap([H | T], U).

upper_(_, A, U)
 => U = A.
