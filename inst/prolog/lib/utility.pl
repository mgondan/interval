/** <file> Utility predicates

Depends on these externally defined predicates:
%   - eval/1
%   - eval/2
*/
:- use_module(variations).

mixed(L, U) :-
    eval(L < 0),
    eval(U > 0).

positive(L, U) :-
    eval(L >= 0),
    eval(U > 0).

zeropos(L, U) :-
    eval(L =:= 0),
    eval(U > 0).

strictpos(L, _) :-
    eval(L > 0).

negative(L, U) :-
    eval(L < 0),
    eval(U =< 0).

zeroneg(L, U) :-
    eval(L < 0),
    eval(U =:= 0).

strictneg(_, U) :-
    eval(U < 0).

zero(L, U) :-
    eval(L =:= 0),
    eval(U =:= 0).

even(A) :-
    eval(A mod 2 =:= 0).

natural(A) :-
    is_of_type(nonneg, A).

floor(A, Dig, Res) :-
    eval(10^Dig, Mul),
    eval(floor(A * Mul) / Mul, Res).

ceiling(A, Dig, Res) :-
    eval(10^Dig, Mul),
    eval(ceiling(A * Mul) / (Mul), Res).

% Min of list 
min_list([H | T], Min) :-
    min_list_(T, H, Min).

min_list_([], Min, Min).

min_list_([H | T], Acc, Min) :-
    eval(Min0 is min(H, Acc)),
    min_list_(T, Min0, Min).

% Max of list
max_list([H | T], Max) :-
    max_list_(T, H, Max).

max_list_([], Max, Max).

max_list_([H | T], Acc, Max) :-
    eval(Max0 is max(H, Acc)),
    max_list_(T, Max0, Max).

% Evaluate all combinations of a function call consisting of intervals
% through a single call to R.
% 
% Example:
% ?- rint:eval_min_max(dt, [1...2, 10...20], Res).
% Res = 0.05808721524735696...0.236045649126701.
%
% This will call "dt(#(2, 2, 1, 1), #(20, 10, 20, 10))" in R and construct 
% the final interval from the minimum and the maximum of the return vector from R .
% 
eval_min_max(Func, Args, Res) :-
    eval_min_max(Func, Args, [], Res).

% Add an optional  list of values (not intervals) that are considered
% for finding the minimum and the maximum of the final interval.
% Typically used to add the maximum of a function that was computed separately. 
eval_min_max(Func, Args, Opt, Res) :-
    arrange_args(Args, Args1),
    compound_name_arguments(Expr, Func, Args1),
    eval(r(Expr), Res1),
    compound_name_arguments(Res1, ##, Res2),
    option(mode(Mode), Opt, []),
    append(Mode, Res2, Res3),
    min_list(Res3, L),
    max_list(Res3, U),
    Res = L...U.

arrange_args(Args0, Args) :-
    length(Args0, N),
    maplist(arg_list, Args0, Args1),
    variations(N, [], Args1, Args2), 
    arrange(Args2, N, Args3),
    maplist(list_vector, Args3, Args4),
    maplist(name_args, Args4, Args).

arg_list(A...A, List) :- 
    !, List = [A].

arg_list(L...U, List) :- 
    !, List = [L, U].

arg_list(A, [A]).

list_vector([true | _], Term) :-
    !, Term = true.

list_vector([false | _], Term) :-
    !, Term = false.

list_vector([Name=true | _], Term) :-
    !, Term = (Name=true).

list_vector([Name=false | _], Term) :-
    !, Term = (Name=false).

list_vector(List, Term) :-
    compound_name_arguments(Term, ##, List).

name_args(Vector, Args) :-
    compound(Vector),
    compound_name_arguments(Vector, ##, [Name=_Value | _]),
    !, compound_name_arguments(Args, =, [Name, Vector]).

name_args(Vector, Vector).

% From lists representing variations build separate lists containing 
% the elements that were at the same position. 
%
% Example:
% rint:arrange([[1, 3], [1, 4], [2, 3], [2, 4]], 2, Args).
% Args = [[2, 2, 1, 1], [4, 3, 4, 3]].
%
arrange(Variations, N, Args) :-
    arrange_(Variations, N, [], Args).

arrange_(_, 0, Acc, Acc) :- !.

arrange_(Variations, N, Acc, Args) :-
    select_args(Variations, N, [], Args1),
    N1 is N - 1,
    arrange_(Variations, N1, [Args1 | Acc], Args).

select_args([], _, Acc, Acc).

select_args([H | T], N, Acc, L) :-
    nth1(N, H, Element),
    select_args(T, N, [Element | Acc], L).

% Convenience function for calling the 'optimize' function in R
% The first argument must be an interval. In case of intervals in
% the remaining arguments, these are combined, and the optimize function 
% is called for each combination.
%
% Example:  
% optimize_(dt, [A1...A2, Df1...Df2, Ncp1...Ncp2, Log], true, Values).
%
% calls:
% eval(r(optimize(dt, #(A1, A2), Df1, Ncp1, Log, maximum=true)), _)
% eval(r(optimize(dt, #(A1, A2), Df2, Ncp1, Log, maximum=true)), _)
% eval(r(optimize(dt, #(A1, A2), Df1, Ncp2, Log, maximum=true)), _)
% eval(r(optimize(dt, #(A1, A2), Df2, Ncp2, Log, maximum=true)), _)
%
% and collects the results as list in 'Values'
optimize_(Fun, [A1...A2 | Args0], Max, Values) :-
    length(Args0, N),
    maplist(arg_list, Args0, Args1),
    variations(N, [], Args1, Args2), 
    maplist(f(Fun, #(A1, A2), Max), Args2, Values).

f(Fun, Range, Max, Args0, Y) :-
    append([Fun, Range], Args0, Args1),
    append(Args1, [maximum=Max], Args2),
    compound_name_arguments(Optimize, optimize, Args2),
    eval(r(Optimize), [_, _-Y]).

