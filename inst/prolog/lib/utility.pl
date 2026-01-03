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
    arrange_args(Args, Args1),
    compound_name_arguments(Expr, Func, Args1),
    eval(r(Expr), Res1),
    compound_name_arguments(Res1, ##, Res2),
    min_list(Res2, L),
    max_list(Res2, U),
    Res = L...U.

arrange_args(Args0, Args) :-
    length(Args0, N),
    maplist(arg_list, Args0, Args1),
    variations(N, [], Args1, Args2), 
    arrange(Args2, N, Args3),
    maplist(list_hashtag, Args3, Args4),
    maplist(name_args, Args4, Args).

arg_list(A...A, List) :- 
    !, List = [A].

arg_list(L...U, List) :- 
    !, List = [L, U].

arg_list(A, List) :- 
    !, List = [A].

list_hashtag(List, Term) :-
    compound_name_arguments(Term, #, List).

name_args(Vector, Args) :-
    compound_name_arguments(Vector, #, [Name=_Value | _]),
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
