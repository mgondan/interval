/** <file> Utility predicates

Depends on these externally defined predicates:
%   - eval/1
%   - eval/2
*/

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
