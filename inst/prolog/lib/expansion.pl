:- module(expansion, [macro_clause/3, macro_clause/4, macro_clause/5, op(150, xfx, ...)]).

/** <file> Macro expansion

Used by user:term_expansion/2 for resolving macros. 
*/

%% macro_clause(+Op/Arity, +Type, ?Clauses) 
%  
%  Expands to interval_/3 clauses having the functor 'Op' and a number of arguments equal to 'Arity' in the head 
%  with possible variations between the types 'number' and '...' (interval). The variation of only 'number' and 
%  '...' is not generated. The latter must be explicitly defined.
%  The body of all clauses contain a call to interval_/3 where 'number' arguments are mapped to intervals. 
%  The only supported type is currently 'mixed'.
%
%  ?- macro_clause((*)/2, mixed, Clauses).
%     Clauses = [ (interval_(A...B * number(C), Res, _) :-
%                   !, interval_(A...B * C...C, Res, _)),
%                 (interval_(number(A) * C...D, Res, _) :-
%                   !, interval_(A...A * C...D, Res, _))
%               ]. 
%
macro_clause(Op/Arity, mixed, Clauses) :-
    findall(Args, variation(Arity, [..., number], Args), HeadArgs0),
    include(memberchk(...), HeadArgs0, HeadArgs1),
    include(memberchk(number), HeadArgs1, HeadArgs2),
    maplist(maplist(instantiate), HeadArgs2, HeadArgs3),
    maplist(maplist(number_interval), HeadArgs3, BodyArgs),
    maplist(args_clause(Op), HeadArgs3, BodyArgs, Clauses).

%% macro_clause(+Op/Arity, +Type, extra(+ExtraArg, +Index), ?Clauses) 
%  
%  As macro_clause/3 with the possibility to add the argument 'ExtraArg' at position 'Index'. 
%  'Arity' includes the extra argument.
%
%  ?- macro_clause(pt/3, mixed, extra(bool(Tail), 3), Clauses).
%     Clauses = [ (interval_(pt(A...B, number(C), bool(Tail)), Res, _) :-
%                   !, interval_(pt(A...B, C...C, bool(Tail)), Res, _)),
%                 (interval_(pt(number(A), C...D, bool(Tail)), Res, _) :-
%                   !, interval_(pt(A...A, C...D, bool(Tail)), Res, _))
%               ].
%
macro_clause(Op/Arity0, mixed, extra(Type, Index), Clauses) :-
    Arity is Arity0 - 1,
    findall(Args, variation(Arity, [..., number], Args), HeadArgs0),
    include(memberchk(...), HeadArgs0, HeadArgs1),
    include(memberchk(number), HeadArgs1, HeadArgs2),
    maplist(maplist(instantiate), HeadArgs2, HeadArgs3),
    maplist(maplist(number_interval), HeadArgs3, BodyArgs0),
    maplist(nth1_(Index, Type), HeadArgs3, HeadArgs),
    maplist(nth1_(Index, Type), BodyArgs0, BodyArgs),
    maplist(args_clause(Op), HeadArgs, BodyArgs, Clauses).

%% macro_clause(+Op/Arity, +Fn/Arity, +Dir, ?Clauses) 
%  
%  Expands to interval_/3 clauses having the functor 'Op' and a number of arguments equal to 'Arity' in the head 
%  with possible variations between the types 'number' and '...' (interval). The variation of only 'number' 
%  is not generated.
%  The body of all clauses contain two calls to Fn/Arity, one for the lower bound of the result interval and 
%  one for the upper bound. Which bounds of the interval arguments in the head are used are determined by 'Dir'.
%  'Dir' is a list of length equal to 'Arity' and each element corresponds to one argument of 'Op' in the head.
%  Symbols usable in 'Dir':
%       + : Monotonincally increasing
%           the lower bound of the argument is used for lower bound of the result
%           the upper bound of the argument is used for upper bound of the result
%       - : Monotonincally decreasing
%           the upper bound of the argument is used for lower bound of the result
%           the lower bound of the argument is used for upper bound of the result
%
%   ?- macro_clause((-)/2, minus/2, [+, -], Clauses).
%      Clauses = [ (interval_(A...B - C...D, Res, _) :-
%                      minus(A, D, L), 
%                      minus(B, C, U), 
%                       !, Res = L...U), 
%                   (interval_(A...B - number(C), Res, _) :-
%                       minus(A, C, L), 
%                       minus(B, C, U), 
%                       !, Res = L...U), 
%                   (interval_(number(A) - B...C, Res, _) :-
%                       minus(A, C, L), 
%                       minus(A, B, U), 
%                       !, Res = L...U)
%                 ].
%
% Special case arity = 1: only one clause of interval_3 is generated
macro_clause(Op/1, Fn/1, [Dir], Clause) :-
    Expr1 =.. [Op, A...B],
    lower(Dir, A...B, Lower),
    upper(Dir, A...B, Upper),
    LowerTerm =.. [Fn, Lower, L],
    UpperTerm =.. [Fn, Upper, U],
    Clause = (interval_(Expr1, Res, _Flags) :-
                LowerTerm,
                UpperTerm,
                !, Res = L...U). 

% General case
macro_clause(Op/Arity, Fn/Arity, Dir, Clause) :-
    findall(Args, variation(Arity, [..., number], Args), Bag0),  
    include(memberchk(...), Bag0, Bag1), 
    maplist(maplist(instantiate), Bag1, Bag2), 
    maplist(bounds(Dir), Bag2, LowerTerm, UpperTerm), 
    maplist(terms_clause(Op, Fn), Bag2, LowerTerm, UpperTerm, Clause).

%% macro_clause(+Op/Arity2, +Fn/Arity, extra(+ExtraArg, +Index), +Dir, ?Clauses) 
%  
%  As previous predicate with the possibility to add the argument 'ExtraArg' at position 'Index'. 
%  'Arity' does not include the extra argument.
%
%   ?- macro_clause(func/3, func_/2, extra(bool(false), 3), [+, -], Clauses).
%      Clauses = [ (interval_(func(A...B, C...D, bool(false)), Res, _) :-
%                       func_(A, D, L), 
%                       func_(B, C, U), 
%                       !, Res = L...U), 
%                   (interval_(func(A...B, number(C), bool(false)), Res, _) :-
%                       func_(A, D, L), 
%                       func_(B, C, U), 
%                       !, Res = L...U), 
%                   (interval_(func(number(A), B...C, bool(false)), Res, _) :-
%                       func_(A, C, L), 
%                       func_(A, B, U), 
%                       !, Res = L...U)
%                 ].
%
macro_clause(Op/_Arity2, Fn/Arity, extra(Type, Index), Dir, Clause) :-
    findall(Args, variation(Arity, [..., number], Args), Bag0),  
    include(memberchk(...), Bag0, Bag1), 
    maplist(maplist(instantiate), Bag1, Bag2), 
    maplist(bounds(Dir), Bag2, LowerTerm, UpperTerm), 
    maplist(nth1_(Index, Type), Bag2, Bag3),
    maplist(terms_clause(Op, Fn), Bag3, LowerTerm, UpperTerm, Clause).

nth1_(Index, Elem, Rest, List) :-
    nth1(Index, List, Elem, Rest).

terms_clause(Op, Fn, Expr0, LowerTerm0, UpperTerm0, Clause) :-
    Expr1 =.. [Op | Expr0],
    append(LowerTerm0, [L], LowerTerm1),
    append(UpperTerm0, [U], UpperTerm1),
    LowerTerm2 =.. [Fn | LowerTerm1],
    UpperTerm2 =.. [Fn | UpperTerm1],
    Clause = (interval_(Expr1, Res, _Flags) :-
                LowerTerm2,
                UpperTerm2,
                !, Res = L...U).

args_clause(Op, HeadArgs, BodyArgs, Clause) :-
    HeadExpr =.. [Op | HeadArgs],
    BodyExpr =.. [Op | BodyArgs],
    Clause = (interval_(HeadExpr, Res, Flags) :-
                !, interval_(BodyExpr, Res, Flags)).
    
instantiate(..., _..._).
instantiate(number, number(_)).

number_interval(number(A), A...A).
number_interval(A...B, A...B).

%% variation(N, List, Variation)
%   
%   Create a variation of length 'N' using the elements in 'List'.
%
%   ?- variation(2, [..., number], L).
%       L = [..., ...] ;
%       L = [..., number] ;
%       L = [number, ...] ;
%       L = [number, number] ;
%       false.
variation(0, _, []).
variation(N, L, [H | T]):-
    N>0,
    N1 is N-1,
    select(H, L, _),
    variation(N1, L, T).

% Obtain lower and upper bounds depending on monotonicity
bounds(Dir, Args, LowerArgs, UpperArgs) :-
    maplist(lower, Dir, Args, LowerArgs),
    maplist(upper, Dir, Args, UpperArgs).

lower(+, A..._, L)
 => L = A.

lower(-, _...A, L)
 => L = A.

% Not supported yet
/* lower(*, A...B, L)
 => L = A ; L = B. */

lower(_, number(A), L)
 => L = A.

upper(+, _...B, U)
 => U = B.

upper(-, A..._, U)
 => U = A.

% Not supported yet
/* upper(*, A...B, U)
 => U = A ; U = B. */

upper(_, number(A), U)
 => U = A.



    