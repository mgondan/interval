:- module(variations, [variations/3, variations/4, n_varlist/3]).

%% variations(N, Elements, Variation)
%   
%   Find all variations of length 'N' using the elements in 'Elements'.
%
%   ?- variations(2, [..., number], Variations).
%       Variations = [[..., ...], [..., number], [number, ...], [number, number]].
variations(N, Elements, Variations) :-
   length(Elements, N1),
   N >= N1,
   !,
   n_varlist(N, [], List),
   variations_(N, Elements, List, Variations).

variations(_, _, _) :-
   writeln("Error: the length of the variation must be equal or higher the number of elements."),
   fail.

variations(N, Elements, Pattern, Variations) :-
   length(Pattern, N2),
   N =:= N2,
   !,
   variations_(N, Elements, Pattern, Variations).

variations(_, _, _, _) :-
   writeln("Error: the length of the variation must be equal the length of the pattern."),
   fail.

n_varlist(0, Acc, Acc) :- !.
n_varlist(N0, Acc, List) :-
   N0 > 0,
   N is N0 - 1,
   n_varlist(N, [_ | Acc], List).

%% variations(N, Elements, Pattern, Variation)
%   
%   Find all variations of length 'N' according to the list 'Pattern'. Each slot of the pattern can be specified by a term (e.g., 'number'), an unbound variable, or 
%   a list of terms (e.g., '[number, ...]') specifying the possible terms for that slot. The term 'list' is for defining lists.
%   Unbound variables are instantied to 'Elements'.
%
%   ?- variations(2, [number, ...], [_, [string, number]], L),
%      L = [[..., string], [number, string], [..., number], [number, number]],
%
variations_(N, Elements, Pattern0, Variations) :-
   maplist(instantiate_pattern_(Elements), Pattern0, Pattern),
   findall(Variation, variation_(N, Pattern, [], Variation), Variations0),
   include(matches(Pattern), Variations0, Variations).

instantiate_pattern_(Elements, Slot0, Slot),
    var(Slot0)
 => Slot = Elements.

instantiate_pattern_(_Elements, [H | T], Slot)
 => Slot = [H | T].

instantiate_pattern_(_Elements, list, Slot)
 => Slot = [_].

instantiate_pattern_(_Elements, Slot0, Slot)
 => Slot = [Slot0].


variation_(0, [], L0, L) :-
   reverse(L0, L).

variation_(N, [H | T], List, Res) :-
    N>0,
    N1 is N-1,
    select(Element, H, _),
    variation_(N1, T, [Element | List], Res).

matches(Pattern, Variation) :-
   maplist(memberchk, Variation, Pattern).
