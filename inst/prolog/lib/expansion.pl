:- module(expansion, [macro_clause/2, macro_clause/3, macro_clause/4, macro_clause/5, op(150, xfx, ...)]).

:- use_module(variations).

/** <module> Macro expansion

Used by user:term_expansion/2 for resolving macros and generate interval_/3 clauses.
*/

% No options
macro_clause(Atomic, Clause)
 => macro_clause(Atomic, [], Clause).

macro_clause(Op/Arity, Fn, Dir, Clause)
 => macro_clause(Op/Arity, Fn, Dir, [], Clause).

%% macro_clause(+Atom, +Options, ?Clauses) 
%  
%  Expands to an interval_/3 clauses having 'Atom' wrapped into 'atomic' and calling eval/2. 
%  This is mainly meant for R variables which also require the respective hook (second example)
%
%  ?- macro_clause(n, Clauses)
%     Clauses = [ (interval_(atomic(n), Res, _) :-
%                  eval(n, Res0),
%                  !, clean(Res0, Res))
%               ]. 
%
%  ?- macro_clause(n, [hook(r_session:r_topic)], Clauses),
%     Clauses = [ (interval_(atomic(n), Res, _) :-
%                  eval(hook(r_session:r_topic, n), Res0),
%                  !, clean(Res0, Res))
%               ].
%
macro_clause(Atomic, Options, Clause)
 => caller_predicate(Atomic, Term, Options),
    Clause0 = (interval_(atomic(Atomic), Res, _) :-
                eval(Term, Res0),
                !, clean(Res0, Res)),
    prefix(Options, Clause0, Clause1),
    Clause = [Clause1].

%% macro_clause(+Op/Arity, interval_, [], ?Options, ?Clauses) 
%  
%  Expands to interval_/3 clauses having the functor 'Op' and a number of arguments equal to 'Arity' in the head 
%  with possible variations between the types 'number' and '...' (interval). The variation of only 'number' and 
%  '...' is not generated. The latter must be explicitly defined.
%  The body of all clauses contain a call to interval_/3 where 'number' arguments are mapped to intervals. 
%  The only supported type is currently 'mixed'.
%
%  ?-  macro_clause((*)/2, interval_, [], Clauses),
%    Clauses = [ (interval_(A...B * number(C), Res, _) :-
%                  !, interval_(A...B * C...C, Res, _)),
%                (interval_(number(A) * C...D, Res, _) :-
%                  !, interval_(A...A * C...D, Res, _))
%              ].
%
macro_clause(Op/Arity, interval_, [], Options, Clause)
 => default_elements(Arity, interval_, Elements),
    option_(Arity, Pattern, Options),
    variations(Arity, Elements, Pattern, HeadArgs0),
    include(memberchk(...), HeadArgs0, HeadArgs1),
    include(memberchk(number), HeadArgs1, HeadArgs2),
    maplist(maplist(instantiate), HeadArgs2, HeadArgs),
    maplist(maplist(number_interval), HeadArgs, BodyArgs),
    maplist(terms_clause(Options, Op), HeadArgs, BodyArgs, Clause0),
    maplist(prefix(Options), Clause0, Clause).

%% macro_clause(+Op/Arity, all, +Dir, ?Options, ?Clauses) 
%  
%  Expands to interval_/3 clauses having the functor 'Op' and a number of arguments equal to 'Arity' in the head 
%  with possible variations between the types 'number' and '...' (interval). 
%  The body of all clauses contain two calls to eval, one for the lower bound of the result interval and 
%  one for the upper bound. Refer to the next comment section for details on how these are determined.
%
%    macro_clause((+)/2, all, [+, +], Clauses),
%    Clauses = [ (interval_(A...B + C...D, Res, _):-
%                  eval(A + C, L),
%                  eval(B + D, U),
%                  !, Res = L...U),
%                (interval_(A...B + number(C), Res, _):-
%                  eval(A + C, L),
%                  eval(B + C, U),
%                  !, Res = L...U),
%                (interval_(number(A) + B...C, Res, _):-
%                  eval(A + B, L),
%                  eval(A + C, U),
%                  !, Res = L...U),
%                (interval_(number(A) + number(B), Res, _) :-
%                  eval(A + B, Res0),
%                  !, Res = number(Res0))
%              ].
%
macro_clause(Op/Arity, all, Dir, Options, Clause)
 => default_elements(Arity, all, Elements),
    option_(Arity, Pattern, Options),
    variations(Arity, Elements, Pattern, HeadArgs0),
    maplist(maplist(instantiate), HeadArgs0, HeadArgs),
    maplist(bounds(Dir), HeadArgs, LowerTerm, UpperTerm), 
    maplist(terms_clause(Options, Op, all), HeadArgs, LowerTerm, UpperTerm, Clause0),
    maplist(prefix(Options), Clause0, Clause).


%% macro_clause(+Op/Arity, +Fn, +Dir, ?Options, ?Clauses) 
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
%       / : Monotonicity not applicable, the respective argument is 'number(_)' in all clauses. 
%   ?- macro_clause((-)/2, minus/2, [+, -], [], Clauses).
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
macro_clause(Op/Arity, Fn, Dir, Options, Clause)
 => default_elements(Arity, Fn, Elements),
    option_(Arity, Pattern, Options),
    variations(Arity, Elements, Pattern, HeadArgs0), 
    include(memberchk(...), HeadArgs0, HeadArgs1), 
    maplist(maplist(instantiate), HeadArgs1, HeadArgs), 
    maplist(bounds(Dir), HeadArgs, LowerTerm, UpperTerm), 
    maplist(terms_clause(Options, Op, Fn), HeadArgs, LowerTerm, UpperTerm, Clause0),
    maplist(prefix(Options), Clause0, Clause).

%
% Supported options:
%
% - prefix(Prefix): adds a module prefix to the body predicates.  
%    ?- macro_clause((-)/1, unary_negate, [+], [prefix(rint)], Clauses),
%       Clauses = [ (interval_(-(A...B), Res, _) :-
%                    rint:unary_negate(A, L), 
%                    rint:unary_negate(B, U), 
%                    !, Res = L...U)
%                 ].
%
% - pattern(Pattern): pattern for the arguments. Each slot of the pattern can be specified by a term (e.g., 'number'), an unbound variable, or 
%   a list of terms (e.g., '[number, ...]') defining the possible terms for that slot. The term 'list' is for defining lists.
%   Unbound variables (i.e, '_' are instantied to '[...]', in case Arity = 1, '[..., number]' in all other cases.
%
%     ?- macro_clause(func/3, func_, [+, -, /], [pattern([_, _, [bool(false), bool(true)]]), prefix(interval)], Clauses),
%           Clauses = [ (interval_(func(A...B, C...D, bool(false)), Res, _) :-
%                          interval:func_(A, D, false, L), 
%                          interval:func_(B, C, false, U), 
%                          !, Res = L...U), 
%                       (interval_(func(A...B, C...D, bool(true)), Res, _) :-
%                          interval:func_(A, D, true, L), 
%                          interval:func_(B, C, true, U), 
%                          !, Res = L...U), 
%                       (interval_(func(A...B, number(C), bool(false)), Res, _) :-
%                          interval:func_(A, D, false, L), 
%                          interval:func_(B, C, false, U), 
%                          !, Res = L...U), 
%                       (interval_(func(A...B, number(C), bool(true)), Res, _) :-
%                          interval:func_(A, D, true, L), 
%                          interval:func_(B, C, true, U), 
%                          !, Res = L...U), 
%                       (interval_(func(number(A), B...C, bool(false)), Res, _) :-
%                          interval:func_(A, C, false, L), 
%                          interval:func_(A, B, false, U), 
%                          !, Res = L...U),
%                       (interval_(func(number(A), B...C, bool(true)), Res, _) :-
%                          interval:func_(A, C, true, L), 
%                          interval:func_(A, B, true, U), 
%                          !, Res = L...U)
%                    ].
%
% - hook(CallerPredicate): the expression for evaluation is wrapped inside a hook. It is only usable with macros
%                          for atomics and of type 'all'.
%    ?- macro_clause(choose/2, all, [+, +], [hook(r_session:r_topic)], Clauses),
%        Clauses = [ (interval_(choose(A...B, C...D), Res, _):-
%                       eval(hook(r_session:r_topic, choose(A, C)), L),
%                       eval(hook(r_session:r_topic, choose(B, D)), U),
%                       !, Res = L...U),
%                    (interval_(choose(A...B, number(C)), Res, _):-
%                       eval(hook(r_session:r_topic, choose(A, C)), L),
%                       eval(hook(r_session:r_topic, choose(B, C)), U),
%                       !, Res = L...U),
%                    (interval_(choose(number(A), B...C), Res, _):-
%                       eval(hook(r_session:r_topic, choose(A, B)), L),
%                       eval(hook(r_session:r_topic, choose(A, C)), U),
%                       !, Res = L...U),
%                    (interval_(choose(number(A), number(B)), Res, _) :-
%                       eval(hook(r_session:r_topic, choose(A, B)), Res0),
%                       !, Res = number(Res0))
%                   ].
%

default_elements(Arity, Fn, Elements),
   Arity =:= 1,
   dif(Fn, all)
 => Elements = [...].

default_elements(_Arity, _Fn, Elements)
 => Elements = [..., number].

option_(_Arity, Pattern, Options),
    member(pattern(Pattern0), Options)
 => Pattern = Pattern0.

option_(Arity, Pattern, _Options)
 => n_varlist(Arity, [], Pattern).

prefix(Options, Clause0, Clause),
    option(prefix(Module), Options)
 => Clause0 =.. [Neck, Head, Body0],
    prefix_(Module, Body0, Body),
    Clause =.. [Neck, Head, Body].

prefix(_Options, Clause0, Clause)
 => Clause = Clause0.
    
prefix_(Module, Body0, Body),
    Body0 = (Lower, Upper, !, clean(A, B))
 => Body = (Module:Lower, Module:Upper, !, Module:clean(A, B)).

prefix_(Module, Body0, Body),
    Body0 = (Lower, Upper, !, Res)
 => Body = (Module:Lower, Module:Upper, !, Res).

prefix_(Module, Body0, Body),
    Body0 = (BodyTerm, !, clean(A, B))
 => Body = (Module:BodyTerm, !, Module:clean(A, B)).

prefix_(Module, Body0, Body),
    Body0 = (BodyTerm, !, Res)
 => Body = (Module:BodyTerm, !, Res).

prefix_(Module, Body0, Body),
    Body0 = (!, BodyTerm)
 => Body = (!, Module:BodyTerm).

caller_predicate(Term0, Term, Options),
    option(hook(r), Options)
 => Term =.. [r, Term0].

caller_predicate(Term0, Term, Options),
    option(hook(Predicate), Options)
 => Term =.. [hook, Predicate, Term0].

caller_predicate(Term0, Term, _Options)
 => Term = Term0.

terms_clause(Options, Op, all, HeadArgs, Args, _ArgsUpper, Clause),
    \+ memberchk_(_..._, HeadArgs)
 => HeadTerm =.. [Op | HeadArgs],
    BodyTerm0 =.. [Op | Args],
    caller_predicate(BodyTerm0, BodyTerm1, Options),
    BodyTerm = (eval(BodyTerm1, Res0)),
    Clause = (interval_(HeadTerm, Res, _Flags) :-
                BodyTerm,
                !, Res = number(Res0)).

terms_clause(Options, Op, all, HeadArgs, ArgsLower, ArgsUpper, Clause)
 => HeadTerm =.. [Op | HeadArgs],
    TermLower0 =.. [Op | ArgsLower],
    TermUpper0 =.. [Op | ArgsUpper],
    caller_predicate(TermLower0, TermLower1, Options),
    caller_predicate(TermUpper0, TermUpper1, Options),
    TermLower = eval(TermLower1, L),
    TermUpper = eval(TermUpper1, U),
    Clause = (interval_(HeadTerm, Res, _Flags) :-
                TermLower,
                TermUpper,
                !, Res = L...U).

terms_clause(_Options, Op, Fn, HeadArgs, ArgsLower0, ArgsUpper0, Clause)
 => HeadTerm =.. [Op | HeadArgs],
    append(ArgsLower0, [L], ArgsLower1),
    append(ArgsUpper0, [U], ArgsUpper1),
    TermLower =.. [Fn | ArgsLower1],
    TermUpper =.. [Fn | ArgsUpper1],
    Clause = (interval_(HeadTerm, Res, _Flags) :-
                TermLower,
                TermUpper,
                !, Res = L...U).
 
terms_clause(_Options, Op, HeadArgs, BodyArgs, Clause)
 => HeadExpr =.. [Op | HeadArgs],
    BodyExpr =.. [Op | BodyArgs],
    Clause = (interval_(HeadExpr, Res, Flags) :-
                !, interval_(BodyExpr, Res, Flags)).

memberchk_(A, [H | _T]) :-
   =@=(A, H),
   !, true.

memberchk_(A, [_ | T]) :-
   !, memberchk_(A, T).

number_interval(number(A), B) 
 => B = A...A.

number_interval(A, B)
 => B = A.

instantiate(..., B)
 => B = _..._.

instantiate(number, B)
 => B = number(_).

instantiate(string, B)
 => B = string(_).

instantiate(bool, B)
 => B = bool(_).

instantiate(list, B)
 => B = _.

instantiate(A, B)
 => B = A.

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

lower(/, bool(A), L)
 => L = A.

lower(/, string(A), L)
 => L = A.

lower(/, A, L)
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
 
upper(/, bool(A), U)
 => U = A.

upper(/, string(A), U)
 => U = A.

upper(/, A, U)
 => U = A.